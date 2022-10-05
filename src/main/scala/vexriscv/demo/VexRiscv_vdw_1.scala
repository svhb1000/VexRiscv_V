package vexriscv.demo

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.com.jtag.Jtag
import spinal.lib.eda.altera.{InterruptReceiverTag, QSysify, ResetEmitterTag}
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}

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


object VexRiscv_vdw_1{
  def main(args: Array[String]) {
    
    val report = SpinalVerilog{

      //CPU configuration
      val cpuConfig = VexRiscvConfig(
        plugins = List(
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
          ioRange      = _(31 downto 28) === 0xF
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
      )

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
