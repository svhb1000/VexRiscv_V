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
  simulcsr   : Boolean = false,
  iCacheSize : Int = 4096,
  dCacheSize : Int = 4096,
  compressed : Boolean = false,
  imemdw     : Int = 32,
  fullshifter: Boolean = false,
  tightiport : Boolean = false,
  tc_mask    : BigInt = 0xFFFF0000l,
  tc_addr    : BigInt = 0x00010000l,
  reset_vector : BigInt = 0x00000000l
)


case class CsrSimulIf() extends Bundle with IMasterSlave{
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
  var csrsimul : CsrSimulIf = null
  

  override def setup(pipeline: VexRiscv): Unit = {
    csrsimul = master(CsrSimulIf()).setName("simcsr")
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
      opt[Boolean]("compressed")  action { (v, c) => c.copy(compressed = v)   } text("infer cpu with compressed instruction set")
      
      opt[Int]("iCacheSize")     action { (v, c) => c.copy(iCacheSize = v) } text("Set instruction cache size, 0 mean no cache")
      opt[Int]("dCacheSize")     action { (v, c) => c.copy(dCacheSize = v) } text("Set data cache size, 0 mean no cache")
      opt[Int]("imemdw")         action { (v, c) => c.copy(imemdw = v) }     text("Set instruction bus datawidth")
      
      opt[Boolean]("fullshifter")  action { (v, c) => c.copy(fullshifter = v)   } text("use full barrel shifter instead of tiny slower implementation")
      
      
      opt[Boolean]("tightiport")    action { (v, c) => c.copy(tightiport = v)   } text("add a tightly coupled instruction port to the datacache")
      opt[BigInt]("tc_mask")        action { (v, c) => c.copy(tc_mask = v) } text("mask applied toaddress of tightlycoupled ram")
      opt[BigInt]("tc_addr")        action { (v, c) => c.copy(tc_addr = v) } text("address to check for tightlycoupled ram")

      opt[BigInt]("reset_vector")        action { (v, c) => c.copy(reset_vector = v) } text("reset vector")

    }
    val argConfig = parser.parse(args, ArgConfig()).get
    
    val report = SpinalVerilog{

      //CPU configuration
        val plugins = ArrayBuffer[Plugin[VexRiscv]]()
      
        if(argConfig.iCacheSize > 0) {
            val icache = new IBusCachedPlugin(
                  prediction = DYNAMIC_TARGET,
                  historyRamSizeLog2 = 8,
                  resetVector = argConfig.reset_vector,
                  compressedGen = argConfig.compressed,
                  config = InstructionCacheConfig(
                    cacheSize = argConfig.iCacheSize,
                    bytePerLine =32,
                    wayCount = 1,
                    addressWidth = 32,
                    cpuDataWidth = 32,
                    memDataWidth = argConfig.imemdw,
                    catchIllegalAccess = true,
                    catchAccessFault   = true,
                    asyncTagMemory     = false,
                    twoCycleRam        = false,
                    twoCycleCache      = !argConfig.compressed   //true --> must be false when compressedGen is chosen to be true
                  )
                )
            if (argConfig.tightiport) {
                icache.newTightlyCoupledPort(TightlyCoupledPortParameter("iBusTc", a => (a & argConfig.tc_mask) === argConfig.tc_addr))  // a => a(30 downto 28) === 0x0 && a(5)))
            }
            plugins ++= List(icache)
        }      
        else {  
            plugins ++= List(
                new IBusSimplePlugin(
                  resetVector = argConfig.reset_vector,
                  cmdForkOnSecondStage = false,
                  cmdForkPersistence = true, // false, otherwise exception in toAvalon. todo : what dies this mean?
                  prediction = NONE,
                  catchAccessFault = false,
                  compressedGen = true
                )
            )
        }
        if(argConfig.dCacheSize > 0) {
            plugins ++= List (
                new DBusCachedPlugin(
                  config = new DataCacheConfig(
                    cacheSize         = argConfig.dCacheSize,
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
                if (argConfig.tightiport) {
                    new StaticMemoryTranslatorPlugin(
                        ioRange      = a => (a.msb) || ((a & argConfig.tc_mask) === argConfig.tc_addr)
                        //ioRange      = ( _.msb )
                        )
                }
                else {
                    new StaticMemoryTranslatorPlugin(
                       ioRange      = ( _.msb )
                    )
                }
            )
        }
        else {  
            plugins ++= List(
                new DBusSimplePlugin(
                  catchAddressMisaligned = false,
                  catchAccessFault = false
                ))
        }
        
        plugins ++= List(
        new DebugPlugin(ClockDomain.current.clone(reset = Bool().setName("debugReset")), 
                        hardwareBreakpointCount=2),
        new CsrPlugin(//CsrPluginConfig.smallest),
            CsrPluginConfig(
                    catchIllegalAccess = false,
                    mvendorid      = null,
                    marchid        = null,
                    mimpid         = null,
                    mhartid        = null,
                    misaExtensionsInit = 66,
                    misaAccess     = CsrAccess.NONE,
                    mtvecAccess    = CsrAccess.READ_WRITE,
                    mtvecInit      = 0x00000020l,
                    mepcAccess     = CsrAccess.READ_WRITE,
                    mscratchGen    = true,
                    mcauseAccess   = CsrAccess.READ_ONLY,
                    mbadaddrAccess = CsrAccess.READ_ONLY,
                    mcycleAccess   = CsrAccess.READ_ONLY,
                    minstretAccess = CsrAccess.READ_WRITE,
                    ecallGen       = false,
                    wfiGenAsWait   = false,
                    ucycleAccess   = CsrAccess.NONE,
                    uinstretAccess = CsrAccess.NONE
                )),
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

        if (argConfig.fullshifter) {
            new FullBarrelShifterPlugin(earlyInjection = true)
        }
        else {
            new LightShifterPlugin
        },

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
        new YamlPlugin("cpu0.yaml")        
      )
      
      if(argConfig.simulcsr) {
        plugins ++= List(
            new CsrSimulPlugin()
        )
      }

      //Irq controller in csr-space
      plugins ++= List(
        new ExternalInterruptArrayPlugin(
          machineMaskCsrId        = 0xBC0,
          machinePendingsCsrId    = 0xFC0
          //supervisorMaskCsrId     = 0x9C0,  only useful when supervisorGen is true in csr config
          //supervisorPendingsCsrId = 0xDC0
        )
      )

      
      val cpuConfig = VexRiscvConfig(plugins)
      

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
            plugin.timerInterrupt //:= False
              .addTag(InterruptReceiverTag(iBus, ClockDomain.current))
            plugin.softwareInterrupt //:= False
              .addTag(InterruptReceiverTag(iBus, ClockDomain.current))
          }
          case plugin: ExternalInterruptArrayPlugin => {
            plugin.externalInterruptArray
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
