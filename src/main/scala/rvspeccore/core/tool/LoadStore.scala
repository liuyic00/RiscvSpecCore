package rvspeccore.core.tool

import chisel3._
import chisel3.util._

import rvspeccore.core.BaseCore
import rvspeccore.core.spec.instset.csr._

// TODO: Optimize code writing style
class TLBSig()(implicit XLEN: Int) extends Bundle {
  val read  = new TLBMemInfo
  val write = new TLBMemInfo
}
class TLBMemInfo()(implicit XLEN: Int) extends Bundle {
  val valid    = Bool()
  val addr     = UInt(XLEN.W)
  val data     = UInt(XLEN.W)
  val memWidth = UInt(log2Ceil(XLEN + 1).W)
  val access   = Bool()
  val level    = UInt(2.W)
}
class PTWLevel()(implicit XLEN: Int) extends Bundle {
  val valid   = Bool()
  val success = Bool()
  val addr    = UInt(XLEN.W)
  val pte     = UInt(XLEN.W) // FIXME: Just for SV39
}

trait LoadStore extends BaseCore with MMU {
//   def ModeU     = 0x0.U // 00 User/Application
//   def ModeS     = 0x1.U // 01 Supervisor
//   def ModeR     = 0x2.U // 10 Reserved
//   def ModeM     = 0x3.U // 11 Machine
  def iFetch = 0x0.U
  def Load   = 0x1.U
  def Store  = 0x2.U
  def width2Mask(width: UInt): UInt = {
    MuxLookup(width, 0.U(64.W))(
      Seq(
        8.U  -> "hff".U(64.W),
        16.U -> "hffff".U(64.W),
        32.U -> "hffff_ffff".U(64.W),
        64.U -> "hffff_ffff_ffff_ffff".U(64.W)
      )
    )
  }
  def memRead(addr: UInt, memWidth: UInt): UInt = {//至少说明处理过，这就表示应该是对齐访问的然后按照要求进行的，然后再返回相应的值
    if (!config.functions.tlb) {
      val bytesWidth = log2Ceil(XLEN / 8)
      val rOff       = addr(bytesWidth - 1, 0) << 3 // addr(byteWidth-1,0) * 8
      val rMask      = width2Mask(memWidth)
      mem.read.valid    := true.B
      mem.read.addr     := addr
      mem.read.memWidth := memWidth
      (mem.read.data >> rOff) & rMask
    } else {
      val bytesWidth    = log2Ceil(XLEN / 8)
      val rOff          = addr(bytesWidth - 1, 0) << 3 // addr(byteWidth-1,0) * 8
      val rMask         = width2Mask(memWidth)
      val mstatusStruct = now.csr.mstatus.asTypeOf(new MstatusStruct)
      val pv            = Mux(mstatusStruct.mprv.asBool, mstatusStruct.mpp, now.internal.privilegeMode)
      val vmEnable      = now.csr.satp.asTypeOf(new SatpStruct).mode === 8.U && (pv < 0x3.U)
      mem.read.valid := true.B
      when(vmEnable) {
        // mem.read.addr     := AddrTransRead(addr)
        // FIXME: addr 的虚实地址均并非64位 需进一步加以限制
        val (success, finaladdr) = PageTableWalk(addr, Load, pv)
        when(success) {
          mem.read.addr := finaladdr
        }.otherwise {
          raiseException(MExceptionCode.loadPageFault)
        }
      }.otherwise {
        mem.read.addr := addr
      }
      mem.read.memWidth := memWidth
      (mem.read.data >> rOff) & rMask
    }
  }
  def memWrite(addr: UInt, memWidth: UInt, data: UInt): Unit = {
    if (!config.functions.tlb) {
      mem.write.valid    := true.B
      mem.write.addr     := addr
      mem.write.memWidth := memWidth
      mem.write.data     := data
    } else {
      // val pv = Mux(now.csr.mstatus)
      val mstatusStruct = now.csr.mstatus.asTypeOf(new MstatusStruct)
      val pv            = Mux(mstatusStruct.mprv.asBool, mstatusStruct.mpp, now.internal.privilegeMode)
      val vmEnable      = now.csr.satp.asTypeOf(new SatpStruct).mode === 8.U && (pv < 0x3.U)
      // printf("[Debug]Write addr:%x, privilegeMode:%x %x %x %x vm:%x\n", addr, pv, mstatusStruct.mprv.asBool, mstatusStruct.mpp, privilegeMode, vmEnable)
      mem.write.valid := true.B
      when(vmEnable) {
        // TODO: addr's bitwidth is lower than 64 bit, need to be modified
        val (success, finaladdr) = PageTableWalk(addr, Store, pv)
        when(success) {
          mem.write.addr := finaladdr
        }.otherwise {
          raiseException(MExceptionCode.storeOrAMOPageFault)
        }
      }.otherwise {
        mem.write.addr := addr
      }
      mem.write.memWidth := memWidth
      mem.write.data     := data
    }
  }

  def iFetchTrans(addr: UInt): (Bool, UInt) = {
    val vmEnable = now.csr.satp.asTypeOf(new SatpStruct).mode === 8.U && (now.internal.privilegeMode < 0x3.U)
    // printf("[Debug]iFetchTrans addr:%x, vm:%x \n", addr, vmEnable)
    val resultStatus = Wire(Bool())
    val resultPC     = Wire(UInt(XLEN.W))
    when(vmEnable) {
      val (success, finaladdr) = PageTableWalkIFetch(addr)
      when(success) {
        // vm 转换成功
        resultPC     := finaladdr
        resultStatus := true.B
        // printf("[Debug]iFetchTrans2 Final Addr: %x\n", finaladdr)
      }.otherwise {
        resultPC     := 0.U
        resultStatus := false.B
        // printf("[Debug]iFetchTrans3 Final Addr Trans Fault \n")
        raiseException(MExceptionCode.instructionPageFault)
      }
    }.otherwise {
      resultPC     := addr
      resultStatus := true.B
    }
    (resultStatus, resultPC)
  }
}

trait MMU extends BaseCore with ExceptionSupport {
  def PARead(addr: UInt, memWidth: UInt): UInt = {
    mem.read.valid    := true.B
    mem.read.addr     := addr
    mem.read.memWidth := memWidth
    mem.read.data
  }

  def PAReadMMU(addr: UInt, memWidth: UInt, no: Int): UInt = {
    tlb.get.Anotherread(no).valid    := true.B
    tlb.get.Anotherread(no).addr     := addr
    tlb.get.Anotherread(no).memWidth := memWidth
    // printf("[Debug] Level: %x Addr: %x Data: %x\n", no.asUInt, addr, tlb.Anotherread(no).data)
    tlb.get.Anotherread(no).data
  }

  def PAWrite(addr: UInt, memWidth: UInt, data: UInt): Unit = {
    mem.write.valid    := true.B
    mem.write.addr     := addr
    mem.write.memWidth := memWidth
    mem.write.data     := data
  }
  def PAWriteMMU(addr: UInt, memWidth: UInt, data: UInt): Unit = {
    // 暂时先使用了一个端口 实际上 dirty操作的是最后找到的那个页 不像读页出现的问题
    tlb.get.Anotherwrite(0).valid    := true.B
    tlb.get.Anotherwrite(0).addr     := addr
    tlb.get.Anotherwrite(0).memWidth := memWidth
    tlb.get.Anotherwrite(0).data     := data
  }

  def LegalAddrStep5(isiFetch: Bool): Bool = {
    // For inst translate
    val sum = now.csr.mstatus.asTypeOf((new MstatusStruct)).sum
    sum.asBool || isiFetch
    // true.B
  }

  def LegalAddrStep5(isiFetch: Bool, privMode: UInt, missflag: PTEFlag, accsessType: UInt): Bool = {
    // FIXME: 需要进一步改这个函数 看手册哈
    val mstatus_mxr = now.csr.mstatus.asTypeOf((new MstatusStruct)).mxr.asBool
    val mstatus_sum = now.csr.mstatus.asTypeOf((new MstatusStruct)).sum.asBool
    // val permCheck = missflag.v && !(pf.privilegeMode === ModeU && !missflag.u) && !(pf.privilegeMode === ModeS && missflag.u && (!pf.status_sum || ifecth))
    val permCheck =
      missflag.v && !(privMode === ModeU && !missflag.u) && !(privMode === ModeS && missflag.u && (!mstatus_sum || isiFetch))
    val permExec  = permCheck && missflag.x
    val permLoad  = permCheck && (missflag.r || mstatus_mxr && missflag.x)
    val permStore = permCheck && missflag.w
    (isiFetch && permExec) || (!isiFetch && ((permLoad && accsessType === 0x1.U) || (permStore && accsessType === 0x2.U)))
    // true.B
  }

  def ValidPage(PTE: PTEFlag): Bool = {
    PTE.r | PTE.x
  }

  def LegalPage(PTE: PTEFlag, level: Int): Bool = {
    ~((!PTE.v | (!PTE.r && PTE.w)) | (level < 0).asBool)
  }

  def IsWriteDirty(PTE: SV39PTE, PA: UInt) = {
    val FlagPTE    = PTE.flag.asTypeOf(new PTEFlag())
    val FlagPTEnew = 0.U(8.W).asTypeOf(new PTEFlag())
    when(~FlagPTE.a | ~FlagPTE.d) {
      FlagPTEnew   := FlagPTE
      FlagPTEnew.a := true.B
      FlagPTEnew.d := true.B
      val PTEnew = Cat(PTE.reserved.asUInt, PTE.ppn.asUInt, PTE.rsw.asUInt, FlagPTEnew.asUInt)
      // printf("[Debug]Is Dirty!!! Need Write Addr: %x old: %x -> new:%x \n", PA, PTE.asUInt, PTEnew.asUInt)
      PAWriteMMU(PA, 64.U, PTEnew.asUInt)
    }
  }

  def LevelCalc(data: UInt): UInt = {
    MuxLookup(data, 3.U)( // faild
      Seq(
        "b100".U -> 2.U,
        "b010".U -> 1.U,
        "b001".U -> 0.U
      )
    )
  }
  def maskPPN(level: UInt): UInt = {
    val mask = MuxLookup(level, 0.U(44.W))(
      Seq(
        2.U -> "b000000_0000000000_0000000000_111111111_111111111".U,
        1.U -> "b000000_0000000000_0000000000_000000000_111111111".U,
        0.U -> "b000000_0000000000_0000000000_000000000_000000000".U
      )
    )
    mask
  }
  def maskVPN(level: UInt): UInt = {
    val mask = MuxLookup(level, 0.U(44.W))(
      Seq(
        2.U -> "b000000000_111111111_111111111".U,
        1.U -> "b000000000_000000000_111111111".U,
        0.U -> "b000000000_000000000_000000000".U
      )
    )
    mask
  }
  def IsSuperPage(ppn: UInt, level: UInt): Bool = {
    val mask = maskPPN(level)
    // printf("[Debug]SuperPage mask:%x ppn:%x flag:%d\n", mask, ppn, ((mask & ppn) =/= 0.U))
    // assume((mask & ppn) === 0.U)
    (mask & ppn) =/= 0.U
    // false.B
  }

  def AddrRSWLegal(addr: UInt): Bool = {
    // FIXME: 需要修一下
    // 前几位是不是好的 + PMAPMP Check
    val flag = Wire(Bool())
    // when((addr << (64 - 39)) >> (63 - 39) === addr){
    //     flag := true.B
    // }.otherwise{
    //     flag := false.B
    // }
    flag := true.B
    flag
  }

  def PageTableWalk(addr: UInt, accsessType: UInt, privMode: UInt): (Bool, UInt) = {
    // Vaddr 前保留位校验 Begin
    // 失败 则Go bad
    val finalSuccess = Wire(Bool())
    val finaladdr    = Wire(UInt(XLEN.W))
    when(AddrRSWLegal(addr)) {
      // printf("[Debug] Vaddr Legal\n")
      // 三级页表翻译 Begin
      val LevelVec = Wire(Vec(3, new PTWLevel()))
      val SatpNow  = now.csr.satp.asTypeOf((new SatpStruct))
      LevelVec(2).valid := true.B // 第一级肯定要打开
      LevelVec(2).addr  := Cat(Cat(0.U(8.W), Cat(SatpNow.ppn, addr(38, 30))), 0.U(3.W))
      for (level <- 0 to 2) {
        // 循环生成三级页表的处理
        when(LevelVec(2 - level).valid) {
          // printf("[Debug] LevelTest:%d %x\n", (2-level).U, LevelVec(2 - level).valid)
          // 寻页且继续的那个函数 返回第二级的值
          val PTE_PA  = LevelVec(2 - level).addr
          val PTE     = PAReadMMU(LevelVec(2 - level).addr, 64.U, level).asTypeOf(new SV39PTE())
          val PTEFlag = PTE.flag.asTypeOf(new PTEFlag())
          when(!PTEFlag.v || (!PTEFlag.r && PTEFlag.w)) {
            // printf("[Debug] Faild flag1 %x \n",(!PTEFlag.v || (!PTEFlag.r && PTEFlag.w)))
            // 失败了 后面也不继续找了
            if (2 - level - 1 >= 0) {
              LevelVec(2 - level - 1).valid := false.B // 下一级的有效就不用打开了
              LevelVec(2 - level - 1).addr  := 0.U
            }
            LevelVec(2 - level).success := false.B // 这一级的寻找失败了
            LevelVec(2 - level).pte     := 0.U
          }.otherwise {
            when(PTEFlag.r || PTEFlag.x) {
              // 成功了
              if (2 - level - 1 >= 0) {
                // printf("[Debug] Faild flag2\n")
                LevelVec(2 - level - 1).valid := false.B // 下一级的有效就不用打开了
                LevelVec(2 - level - 1).addr  := 0.U
              }
              LevelVec(2 - level).success := true.B // 这一级的寻找成功了
              LevelVec(2 - level).pte     := PTE.asUInt
            }.otherwise {
              // 需要继续找
              if (2 - level - 1 >= 0) {
                LevelVec(2 - level - 1).valid := true.B // 下一级的有效打开
                // FIXME: 需要特别优化
                if ((2 - level - 1) == 1) {
                  LevelVec(2 - level - 1).addr := Cat(Cat(0.U(8.W), Cat(PTE.ppn, addr(29, 21))), 0.U(3.W))
                }
                if ((2 - level - 1) == 0) {
                  LevelVec(2 - level - 1).addr := Cat(Cat(0.U(8.W), Cat(PTE.ppn, addr(20, 12))), 0.U(3.W))
                }
              }
              LevelVec(2 - level).success := false.B // 这一级的寻找失败了
              LevelVec(2 - level).pte     := 0.U
            }
          }
        }.otherwise {
          // printf("[Debug] Faild flag3\n")
          // // 这一级无效 需要把这一级的success 和 下一级的有效信号给干掉
          if (2 - level - 1 >= 0) {
            LevelVec(2 - level - 1).valid := false.B // 下一级的有效关闭
            LevelVec(2 - level - 1).addr  := 0.U
          }
          LevelVec(2 - level).success := false.B
          LevelVec(2 - level).pte     := 0.U

        }
        // when(LevelVec(2 - level).success){
        //     printf("[Debug]LevelTest:%d level success %x\n", (2-level).U, LevelVec(2 - level).success)
        // }
      }
      // printf("[Debug]LevelSuccess : %d %d %d\n", LevelVec(2).success, LevelVec(1).success, LevelVec(0).success)
      // printf("[Debug]LevelPTE     : %x %x %x\n", LevelVec(2).pte, LevelVec(1).pte, LevelVec(0).pte)
      // printf("[Debug]LevelSuccess2: %x\n", Cat(Cat(LevelVec(2).success, LevelVec(1).success), LevelVec(0).success))
      // printf("[Debug]LevelSuccess3: %d\n", LevelCalc(Cat(Cat(LevelVec(2).success, LevelVec(1).success), LevelVec(0).success)))

      // 三级页表翻译 End
      // finalSuccess := LevelVec(2).success || LevelVec(1).success || LevelVec(0).success
      val successLevel = LevelCalc(Cat(Cat(LevelVec(2).success, LevelVec(1).success), LevelVec(0).success))
      when(~(successLevel === 3.U)) {
        // 翻译暂时成功了
        // printf("[Debug] Translate temporarily successful\n")
        // when(LegalAddrStep5(false.B)){
        when(
          LegalAddrStep5(
            false.B,
            privMode,
            LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).flag.asTypeOf(new PTEFlag()),
            accsessType
          )
        ) {
          // 检测超大页
          // printf("[Debug] Step5 Legal\n")
          when(IsSuperPage(LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn, successLevel)) {
            // 是大页
            finalSuccess := false.B
            finaladdr    := 0.U
          }.otherwise {
            // 成功了 但是还需要操作一下Dirty
            // val PTE = PAReadMMU(LevelVec(2).addr, 64.U, 2).asTypeOf(new SV39PTE())
            // printf("[Debug]PTE.d test: Addr:%x PTE:%x\n", LevelVec(successLevel).addr, LevelVec(successLevel).pte)
            when(accsessType === 0x2.U) {
              IsWriteDirty(LevelVec(successLevel).pte.asTypeOf(new SV39PTE()), LevelVec(successLevel).addr)
            }
            finalSuccess := true.B
            // val adada_addr = ((Cat((LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn),0.U(12.W)) & (~maskPPN(successLevel)))) | (addr & maskVPN(successLevel))
            // printf("[Debug]Final success ppn:%x addr:%x trans:%x\n", LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn, addr, adada_addr)
            // finaladdr := "h0000_0000_8000_0000".U | addr
            finaladdr := ((Cat((LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn), addr(11, 0)) & (~maskPPN(
              successLevel
            )))) | (addr & maskVPN(successLevel))
          }
        }.otherwise {
          // 又失败了
          // printf("[Debug] Step5 Faild\n")
          finalSuccess := false.B
          finaladdr    := 0.U
        }
      }.otherwise {
        // 翻译失败了
        finalSuccess := false.B
        finaladdr    := 0.U
      }
      // 这个时候失败是一定失败 成功可不一定成功
    }.otherwise {
      // printf("[Debug] Vaddr illegal\n")
      finalSuccess := false.B
      finaladdr    := 0.U
    }
    // Vaddr 前保留位校验 End
    (finalSuccess, finaladdr)
  }

  def PageTableWalkIFetch(addr: UInt): (Bool, UInt) = {
    // Vaddr 前保留位校验 Begin
    // 失败 则Go bad
    val finalSuccess = Wire(Bool())
    val finaladdr    = Wire(UInt(XLEN.W))
    when(AddrRSWLegal(addr)) {
      // printf("[Debug] Vaddr Legal\n")
      // 三级页表翻译 Begin
      val LevelVec = Wire(Vec(3, new PTWLevel()))
      val SatpNow  = now.csr.satp.asTypeOf((new SatpStruct))
      LevelVec(2).valid := true.B // 第一级肯定要打开
      LevelVec(2).addr  := Cat(Cat(0.U(8.W), Cat(SatpNow.ppn, addr(38, 30))), 0.U(3.W))
      for (level <- 0 to 2) {
        // 循环生成三级页表的处理
        when(LevelVec(2 - level).valid) {
          // printf("[Debug]LevelTest:%d %x\n", (2-level).U, LevelVec(2 - level).valid)
          // 寻页且继续的那个函数 返回第二级的值
          val PTE_PA  = LevelVec(2 - level).addr
          val PTE     = PAReadMMU(LevelVec(2 - level).addr, 64.U, 3 + level).asTypeOf(new SV39PTE())
          val PTEFlag = PTE.flag.asTypeOf(new PTEFlag())
          LevelVec(2 - level).pte := PTE.asUInt
          when(~PTEFlag.v | (~PTEFlag.r && PTEFlag.w)) {
            // 失败了 后面也不继续找了
            if (2 - level - 1 >= 0) {
              LevelVec(2 - level - 1).valid := false.B // 下一级的有效就不用打开了
              LevelVec(2 - level - 1).addr  := 0.U
            }
            LevelVec(2 - level).success := false.B // 这一级的寻找失败了
          }.otherwise {
            when(PTEFlag.r | PTEFlag.x) {
              // 成功了
              if (2 - level - 1 >= 0) {
                LevelVec(2 - level - 1).valid := false.B // 下一级的有效就不用打开了
                LevelVec(2 - level - 1).addr  := 0.U
              }
              LevelVec(2 - level).success := true.B // 这一级的寻找成功了
            }.otherwise {
              // 需要继续找
              if (2 - level - 1 >= 0) {
                LevelVec(2 - level - 1).valid := true.B // 下一级的有效打开
                // FIXME: 需要特别优化
                if ((2 - level - 1) == 1) {
                  LevelVec(2 - level - 1).addr := Cat(Cat(0.U(8.W), Cat(PTE.ppn, addr(29, 21))), 0.U(3.W))
                }
                if ((2 - level - 1) == 0) {
                  LevelVec(2 - level - 1).addr := Cat(Cat(0.U(8.W), Cat(PTE.ppn, addr(20, 12))), 0.U(3.W))
                }
              }
              LevelVec(2 - level).success := false.B // 这一级的寻找失败了
            }
          }
        }.otherwise {
          // // 这一级无效 需要把这一级的success 和 下一级的有效信号给干掉
          if (2 - level - 1 >= 0) {
            LevelVec(2 - level - 1).valid := false.B // 下一级的有效关闭
            LevelVec(2 - level - 1).addr  := 0.U
          }
          LevelVec(2 - level).success := false.B
          LevelVec(2 - level).pte     := 0.U

        }
        // when(LevelVec(2 - level).success){
        //     printf("[Debug]LevelTest:%d level success %x\n", (2-level).U, LevelVec(2 - level).success)
        // }
      }
      // printf("[Debug]LevelSuccess : %d %d %d\n", LevelVec(2).success, LevelVec(1).success, LevelVec(0).success)
      // printf("[Debug]LevelPTE     : %x %x %x\n", LevelVec(2).pte, LevelVec(1).pte, LevelVec(0).pte)
      // printf("[Debug]LevelSuccess2: %x\n", Cat(Cat(LevelVec(2).success, LevelVec(1).success), LevelVec(0).success))
      // printf("[Debug]LevelSuccess3: %d\n", LevelCalc(Cat(Cat(LevelVec(2).success, LevelVec(1).success), LevelVec(0).success)))

      // 三级页表翻译 End
      // finalSuccess := LevelVec(2).success || LevelVec(1).success || LevelVec(0).success
      val successLevel = LevelCalc(Cat(Cat(LevelVec(2).success, LevelVec(1).success), LevelVec(0).success))
      when(~(successLevel === 3.U)) {
        // 翻译暂时成功了
        // printf("[Debug]PTE Success\n")
        when(LegalAddrStep5(true.B)) {
          // 检测超大页
          // printf("[Debug]Legal Address Step5 True\n")
          when(IsSuperPage(LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn, successLevel)) {
            // 是大页
            // printf("[Debug]SuperPage fault\n")
            finalSuccess := false.B
            finaladdr    := 0.U
          }.otherwise {
            // printf("[Debug]PTE.d test: Addr:%x PTE:%x\n", LevelVec(successLevel).addr, LevelVec(successLevel).pte)
            finalSuccess := true.B
            // val adada_addr = ((Cat((LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn),0.U(12.W)) & (~maskPPN(successLevel)))) | (addr & maskVPN(successLevel))
            // printf("[Debug]Final success ppn:%x addr:%x trans:%x\n", LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn, addr, adada_addr)
            // finaladdr := "h0000_0000_8000_0000".U | addr
            finaladdr := ((Cat((LevelVec(successLevel).pte.asTypeOf(new SV39PTE()).ppn), addr(11, 0)) & (~maskPPN(
              successLevel
            )))) | (addr & maskVPN(successLevel))
          }
        }.otherwise {
          // 又失败了
          // printf("[Debug]Legal Address Step5 False\n")
          finalSuccess := false.B
          finaladdr    := 0.U
        }
      }.otherwise {
        // 翻译失败了
        // printf("[Debug]PTE False\n")
        finalSuccess := false.B
        finaladdr    := 0.U
      }
      // 这个时候失败是一定失败 成功可不一定成功
    }.otherwise {
      // printf("[Debug] Vaddr illegal\n")
      finalSuccess := false.B
      finaladdr    := 0.U
    }
    // Vaddr 前保留位校验 End
    (finalSuccess, finaladdr)
  }

}
