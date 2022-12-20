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

import spinal.lib.cpu.riscv.debug.DebugTransportModuleParameter


//vdw_0 : 
// - copy from smallandproductive,
//vdw_1 : 
// - add icache from GenFullNoMmuMaxPerf
// - add mul and div insns
//vdw_2 : 
// - cfu possible
// - tcm possible --> still testing


case class ArgConfig_2(
  simulcsr   : Boolean = false,
  debug_jtag : Boolean = false,
  iCacheSize : Int = 4096,
  dCacheSize : Int = 4096,
  compressed : Boolean = false,
  imemdw     : Int = 32,
  use_cfu    : Boolean = false,
  fullshifter: Boolean = false,
  catch_illegal : Boolean = false,
  //set address of tcm region : first AND with 'mask' then compare with 'addr'. 
  tightiport : Boolean = true,
  tc_mask    : BigInt = 0xFFFF0000l,
  tc_addr    : BigInt = 0x00010000l,
  
  //extra io mask on top of bit31 cache bypass
  io_mask : BigInt = 0xFFFF0000l,
  io_addr : BigInt = 0x00010000l,
  
  reset_vector : BigInt = 0x00000000l
)


case class CsrSimulIf_2() extends Bundle with IMasterSlave{
  val done   = Bool()
  val result = Bool()
  val testnr = Bits(32 bits)
  val stdout = Bits(32 bits)
  
  val stdin        = Bits(32 bits)
  val sim_settings = Bits(32 bits)
  
  override def asMaster(): Unit = {
      out(done, result, testnr, stdout);
      in(stdin, sim_settings)
  }
}

class CsrSimulPlugin_2(simDoneCsrId   : Int = 0x7F0,
                     simResultCsrId   : Int = 0x7F1,
                     simTestNrCsrId   : Int = 0x7F2,
                     simStdOutCsrId   : Int = 0x7F3,
                     simStdInCsrId    : Int = 0x7F4,
                     simSettingsCsrId : Int = 0x7F5) extends Plugin[VexRiscv]{
  var csrsimul : CsrSimulIf_2 = null
  

  override def setup(pipeline: VexRiscv): Unit = {
    csrsimul = master(CsrSimulIf_2()).setName("simcsr")
  }

  override def build(pipeline: VexRiscv): Unit = {
    import pipeline._
    import pipeline.config._

    pipeline plug new Area{
      val doneReg   = Reg(Bool())
      val resultReg = Reg(Bool())
      val testnrReg = Reg(Bits(32 bits))
      
      val stdoutReg = Reg(Bits(32 bits))
      

      val csrService = pipeline.service(classOf[CsrInterface])
      csrService.w(simDoneCsrId,   doneReg)
      csrService.w(simResultCsrId, resultReg)
      csrService.w(simTestNrCsrId, testnrReg)

      csrService.w(simStdOutCsrId, stdoutReg)
      csrService.r(simStdInCsrId, csrsimul.stdin)
      csrService.r(simSettingsCsrId, csrsimul.sim_settings)

      csrsimul.done   := doneReg
      csrsimul.result := resultReg
      csrsimul.testnr := testnrReg
      csrsimul.stdout := stdoutReg
    }
  }
}

object VexRiscv_vdw_2{
  def main(args: Array[String]) {
    
    val parser = new scopt.OptionParser[ArgConfig_2]("VexRiscvGen") {
      opt[Boolean]("simulcsr")    action { (v, c) => c.copy(simulcsr = v)   } text("add csr's for simulation status report")

      opt[Boolean]("debug_jtag")  action { (v, c) => c.copy(debug_jtag = v)   } text("add jtag debug interface")

      opt[Boolean]("compressed")  action { (v, c) => c.copy(compressed = v)   } text("infer cpu with compressed instruction set")
      
      opt[Int]("iCacheSize")     action { (v, c) => c.copy(iCacheSize = v) } text("Set instruction cache size, 0 mean no cache")
      opt[Int]("dCacheSize")     action { (v, c) => c.copy(dCacheSize = v) } text("Set data cache size, 0 mean no cache")
      opt[Int]("imemdw")         action { (v, c) => c.copy(imemdw = v) }     text("Set instruction bus datawidth")
      
      opt[Boolean]("fullshifter")  action { (v, c) => c.copy(fullshifter = v)   } text("use full barrel shifter instead of tiny slower implementation")
      
      opt[Boolean]("catch_illegal")  action { (v, c) => c.copy(catch_illegal = v)   } text("enable catching illegal isntructions")
      
      
      opt[Boolean]("use_cfu")      action { (v, c) => c.copy(use_cfu = v)   } text("instantiate cfu interface port")
      
      opt[Boolean]("tightiport")    action { (v, c) => c.copy(tightiport = v)   } text("add a tightly coupled instruction port to the datacache")
      opt[BigInt]("tc_mask")        action { (v, c) => c.copy(tc_mask = v) } text("AND mask applied to address for tightlycoupled ram")
      opt[BigInt]("tc_addr")        action { (v, c) => c.copy(tc_addr = v) } text("address to check for match after masking address to check on tightly coupled address region")

      opt[BigInt]("io_mask")        action { (v, c) => c.copy(io_mask = v) } text("AND mask applied to address of databus to check on cache bypass")
      opt[BigInt]("io_addr")        action { (v, c) => c.copy(io_addr = v) } text("address to check for match after masking address to check on dcache bypass")

      opt[BigInt]("reset_vector")        action { (v, c) => c.copy(reset_vector = v) } text("reset vector")

    }
    val argConfig = parser.parse(args, ArgConfig_2()).get
    
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
                icache.newTightlyCoupledPort(TightlyCoupledPortParameter
                    ("iBusTc", a => (a & argConfig.tc_mask) === argConfig.tc_addr))
            }
            plugins ++= List(icache)
        }      
        else {  
            plugins ++= List(
                new IBusSimplePlugin(
                  resetVector = argConfig.reset_vector,
                  cmdForkOnSecondStage = false,
                  cmdForkPersistence = true, // false, otherwise exception in toAvalon. todo : what does this mean?
                  prediction = NONE,
                  catchAccessFault = false,
                  compressedGen = argConfig.compressed
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
                )
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
/*             new StaticMemoryTranslatorPlugin(
                    ioRange      = a => (a.msb) || ((a & argConfig.io_mask) ===  argConfig.io_addr)
                    //ioRange      = ( _.msb )
                 ),
  */               
                 
                     new PmpPlugin(
            regions = 16,
            granularity = 32,
            ioRange      = a => (a.msb) || ((a & argConfig.io_mask) ===  argConfig.io_addr)

//            ioRange = _(31 downto 28) === 0xf
            )

        )
        
        
        
        if (argConfig.debug_jtag) {
            //official RISCV debug protocol implementation
            plugins ++= List( new EmbeddedRiscvJtag(
                                      p = DebugTransportModuleParameter(
                                        addressWidth = 7,
                                        version      = 1,
                                        idle         = 7
                                      ),
                                      debugCd = ClockDomain.current.copy(reset = Bool().setName("debugReset")),
                                      withTunneling = false,
                                      withTap = true
                                    )
                            )
        }
        
        
        plugins ++= List(
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
                    uinstretAccess = CsrAccess.NONE,
                    withPrivilegedDebug = argConfig.debug_jtag   //for the official RISCV debug implementation
                )),
        new DecoderSimplePlugin(
          catchIllegalInstruction = argConfig.catch_illegal
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
            new CsrSimulPlugin_2()
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

      //CFU instantiation
      if(argConfig.use_cfu) {
          plugins ++= List(
            new CfuPlugin(
              stageCount = 1,
              allowZeroLatency = true,
              withEnable = false,
              stateAndIndexCsrOffset = -1, //make sure no csr is generated, otherwise an error will be generated
              statusCsrOffset = -1,        //make sure no csr is generated, otherwise an error will be generated
              encodings = List(
                CfuPluginEncoding (
                  instruction = M"-------------------------0-01011", 
                        // bit 5 :     
                            //choose 'custom0', see as I-type insn like 'OP-IMM' (ADDI, SUBI, ...)
                            //choose 'custom1', see as R-type insn like 'OP' (ADD, SUB, ...)
                  functionId = List(14 downto 12),
                  input2Kind = CfuPlugin.Input2Kind.RS
                )
              ),
              busParameter = CfuBusParameter(
                CFU_VERSION = 0,
                CFU_INTERFACE_ID_W = 0,
                CFU_FUNCTION_ID_W = 3,
                CFU_REORDER_ID_W = 0,
                CFU_REQ_RESP_ID_W = 0,
                CFU_INPUTS = 2,
                CFU_INPUT_DATA_W = 32,
                CFU_OUTPUTS = 1,
                CFU_OUTPUT_DATA_W = 32,
                CFU_FLOW_REQ_READY_ALWAYS = false,
                CFU_FLOW_RESP_READY_ALWAYS = false,
                CFU_WITH_STATUS = false,
                CFU_RAW_INSN_W = 32,
                CFU_CFU_ID_W = 0,
                CFU_STATE_INDEX_NUM = 0
              )
            )
          )
      }
      
      val cpuConfig = VexRiscvConfig(plugins)
      

      //CPU instanciation
      val cpu = new VexRiscv(cpuConfig)

      //CPU modifications to be an Avalon one
      cpu.setDefinitionName("VexRiscv_vdw_2")
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
