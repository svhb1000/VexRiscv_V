package vexriscv.demo

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.com.jtag.Jtag
import spinal.lib.eda.altera.{InterruptReceiverTag, QSysify, ResetEmitterTag}
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by spinalvm on 14.07.17.
 */
//class VexRiscvAvalon(debugClockDomain : ClockDomain) extends Component{
//
//}

//vdw_0 : 
// - copy from smallandproductive,
//   changed cmdForkOnSecondStage to true, due to assertion in avalon bus interface
//vdw_1 : 
// - add icache from GenFullNoMmuMaxPerf
// - add mul and div insns


case class ArgConfig(
  simulcsr : Boolean = false
)


case class CsrSimul() extends Bundle with IMasterSlave{
  val done   = Bool()
  val result = Bool()
  val testnr = Bits(32 bits)
  override def asMaster(): Unit = {
      out(done, result, testnr)
  }
}

class CsrSimulPlugin(simDoneCsrId   : Int = 0x7F0,
                     simResultCsrId : Int = 0x7F1,
                     simTestNrCsrId : Int = 0x7F2) extends Plugin[VexRiscv]{
  var csrsimul : CsrSimul = null
  

  override def setup(pipeline: VexRiscv): Unit = {
    csrsimul = master(CsrSimul()).setName("simcsr")
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    pipeline plug new Area{
      val doneReg   = Reg(Bool())
      val resultReg = Reg(Bool())
      val testnrReg = Reg(Bits(32 bits))

      val csrService = pipeline.service(classOf[CsrInterface])
      csrService.w(simDoneCsrId,   doneReg)
      csrService.w(simResultCsrId, resultReg)
      csrService.w(simTestNrCsrId, testnrReg)

      csrsimul.done   := doneReg
      csrsimul.result := resultReg
      csrsimul.testnr := testnrReg
    }
  }
}

object VexRiscv_vdw_1{
  def main(args: Array[String]) {
    
    val parser = new scopt.OptionParser[ArgConfig]("VexRiscvGen") {
      opt[Boolean]("simulcsr")    action { (v, c) => c.copy(simulcsr = v)   } text("add csr's for simulation status report")
    }
    val argConfig = parser.parse(args, ArgConfig()).get
    
    val report = SpinalVerilog{

      //CPU configuration
        val plugins = ArrayBuffer[Plugin[VexRiscv]]()
      
        plugins ++= List(
        //~ new IBusSimplePlugin(
          //~ resetVector = 0x00000000l,
          //~ cmdForkOnSecondStage = false,
          //~ cmdForkPersistence = true, // false, otherwise exception in toAvalon. todo : what dies this mean?
          //~ prediction = NONE,
          //~ catchAccessFault = false,
          //~ compressedGen = false
        //~ ),
        //~ new DBusSimplePlugin(
          //~ catchAddressMisaligned = false,
          //~ catchAccessFault = false
        //~ ),
        new IBusCachedPlugin(
          prediction = DYNAMIC_TARGET,
          historyRamSizeLog2 = 8,
          resetVector = 0x00000000l,
          config = InstructionCacheConfig(
            cacheSize = 4096,
            bytePerLine =32,
            wayCount = 1,
            addressWidth = 32,
            cpuDataWidth = 32,
            memDataWidth = 32,
            catchIllegalAccess = true,
            catchAccessFault = true,
            asyncTagMemory = false,
            twoCycleRam = false,
            twoCycleCache = true
          )
        ),        
        new DBusCachedPlugin(
          config = new DataCacheConfig(
            cacheSize         = 4096,
            bytePerLine       = 32,
            wayCount          = 1,
            addressWidth      = 32,
            cpuDataWidth      = 32,
            memDataWidth      = 32,
            catchAccessError  = true,
            catchIllegal      = true,
            catchUnaligned    = true
          )
        ),
        new StaticMemoryTranslatorPlugin(
          //ioRange      = _(31 downto 28) === 0xF
          ioRange      = _.msb 
        ),
        new CsrPlugin(CsrPluginConfig.smallest),
        new DecoderSimplePlugin(
          catchIllegalInstruction = false
        ),
        new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = false
        ),
        new IntAluPlugin,
        new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = true
        ),
        new LightShifterPlugin,
        new HazardSimplePlugin(
          bypassExecute           = true,
          bypassMemory            = true,
          bypassWriteBack         = true,
          bypassWriteBackBuffer   = true,
          pessimisticUseSrc       = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
        ),
        new MulPlugin,
        new DivPlugin,
        new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = true
        ),
        new YamlPlugin("cpu0.yaml")        )
      

      if(argConfig.simulcsr) {
        plugins ++= List(
            new CsrSimulPlugin()
        )
      }
      
      val cpuConfig = VexRiscvConfig(plugins)
      
      //~ if(argConfig.externalInterruptArray) plugins ++= List(
        //~ new ExternalInterruptArrayPlugin(
          //~ machineMaskCsrId = 0xBC0,
          //~ machinePendingsCsrId = 0xFC0,
          //~ supervisorMaskCsrId = 0x9C0,
          //~ supervisorPendingsCsrId = 0xDC0
        //~ )
      //~ )

      //CPU instanciation
      val cpu = new VexRiscv(cpuConfig)

      //CPU modifications to be an Avalon one
      cpu.setDefinitionName("VexRiscv_vdw_1")
      cpu.rework {
        var iBus : AvalonMM = null
        for (plugin <- cpuConfig.plugins) plugin match {
          case plugin: IBusSimplePlugin => {
            plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
            iBus = master(plugin.iBus.toAvalon())
              .setName("iBusAvalon")
              .addTag(ClockDomainTag(ClockDomain.current)) //Specify a clock domain to the iBus (used by QSysify)
          }
          case plugin: IBusCachedPlugin => {
            plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
            iBus = master(plugin.iBus.toAvalon())
              .setName("iBusAvalon")
              .addTag(ClockDomainTag(ClockDomain.current)) //Specify a clock domain to the iBus (used by QSysify)
          }
          case plugin: DBusSimplePlugin => {
            plugin.dBus.setAsDirectionLess()
            master(plugin.dBus.toAvalon())
              .setName("dBusAvalon")
              .addTag(ClockDomainTag(ClockDomain.current))
          }
          case plugin: DBusCachedPlugin => {
            plugin.dBus.setAsDirectionLess()
            master(plugin.dBus.toAvalon())
              .setName("dBusAvalon")
              .addTag(ClockDomainTag(ClockDomain.current))
          }
          case plugin: DebugPlugin => plugin.debugClockDomain {
            plugin.io.bus.setAsDirectionLess()
            val jtag = slave(new Jtag())
              .setName("jtag")
            jtag <> plugin.io.bus.fromJtag()
            plugin.io.resetOut
              .addTag(ResetEmitterTag(plugin.debugClockDomain))
              .parent = null //Avoid the io bundle to be interpreted as a QSys conduit
          }
          case _ =>
        }
        for (plugin <- cpuConfig.plugins) plugin match {
          case plugin: CsrPlugin => {
            plugin.externalInterrupt
              .addTag(InterruptReceiverTag(iBus, ClockDomain.current))
            plugin.timerInterrupt
              .addTag(InterruptReceiverTag(iBus, ClockDomain.current))
            plugin.softwareInterrupt
              .addTag(InterruptReceiverTag(iBus, ClockDomain.current))
          }
          case _ =>
        }
      }
      cpu
    }

    //Generate the QSys TCL script to integrate the CPU
    QSysify(report.toplevel)
  }
}
