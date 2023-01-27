package Pipelining

import chisel3._
import chisel3.util._

class PipTop extends Module {
    val io = IO (new Bundle{
        val out = Output(SInt(32.W))
        val address = Input(UInt(10.W))
        val data = Output(UInt(32.W))
    })

//objects
val Alu_mod = Module(new ALU)
dontTouch(Alu_mod.io)
val AluControl_mod = Module(new AluControl)
dontTouch(AluControl_mod.io)
val Branch_mod = Module(new Branch)
dontTouch(Branch_mod.io)
val Control_mod = Module(new Control)
dontTouch(Control_mod.io)
val DataMem_mod = Module(new DataMemory)
dontTouch(DataMem_mod.io)
val ImmGen_mod = Module(new ImmGen)
dontTouch(ImmGen_mod.io)
val InstMem = Module(new InstMemory("C:/Users/layba/Downloads/CHISEL3LABS/instmem.txt"))
dontTouch(InstMem.io)
val JalR_mod = Module(new JalR)
dontTouch(JalR_mod.io)
val PC_mod = Module(new PC)
dontTouch(PC_mod.io)
val PC4_mod = Module(new PC4)
dontTouch(PC4_mod.io)
val RegFile_mod = Module(new RegFile)
dontTouch(RegFile_mod.io)
val IFID = Module(new IF_ID)
dontTouch(IFID.io)
val IDEX = Module(new ID_EX)
dontTouch(IDEX.io)
val EXMEM = Module(new EX_MEM)
dontTouch(EXMEM.io)
val MEMWB = Module(new MEM_WB)
dontTouch(MEMWB.io)
val ForwardingUnit = Module(new ForwardUnit)
dontTouch(ForwardingUnit.io)
val Branchforward_mod = Module(new BranchForward)
dontTouch(Branchforward_mod.io)
val HazardDetection_mod = Module(new HazardDetection)
dontTouch(HazardDetection_mod.io)
val Structuralhazard_mod = Module(new StructuralHazard)
dontTouch(Structuralhazard_mod.io)


//IFID
IFID.io.pc_in := PC_mod.io.out
IFID.io.mux_inst_in := InstMem.io.data
IFID.io.pc4_in := PC4_mod.io.out

switch(HazardDetection_mod.io.inst_forward) {
    is(0.B){
        IFID.io.mux_inst_in := InstMem.io.data
    }
    is(1.B){
        IFID.io.mux_inst_in := HazardDetection_mod.io.inst_out
        IFID.io.pc_in := HazardDetection_mod.io.current_pc_out
        
    }
}

//fetch
PC_mod.io.in := PC4_mod.io.out  
PC4_mod.io.pc := PC_mod.io.out
InstMem.io.address := PC_mod.io.out

//Structural Hazard
Structuralhazard_mod.io.rs1 := IFID.io.mux_inst_out(19,15)
Structuralhazard_mod.io.rs2 := IFID.io.mux_inst_out(24,20)
Structuralhazard_mod.io.MEM_WB_Rd := MEMWB.io.MEMWB_rd_out
Structuralhazard_mod.io.MEM_WB_regWr := MEMWB.io.MEMWB_reg_w_out

switch(Structuralhazard_mod.io.fwd_rs1){
    is("b1".U){
        IDEX.io.rs1_in := RegFile_mod.io.write_data
    }
    is("b0".U){
        IDEX.io.rs1_in := RegFile_mod.io.Reg1
    }
}

switch(Structuralhazard_mod.io.fwd_rs2){
    is("b1".U){
        IDEX.io.rs2_in := RegFile_mod.io.write_data
    }
    is("b0".U){
        IDEX.io.rs2_in := RegFile_mod.io.Reg2
    }
}

//Decode
Control_mod.io.opcode := IFID.io.mux_inst_out(6,0)
RegFile_mod.io.Reg1 := IFID.io.mux_inst_out(19,15)
RegFile_mod.io.Reg2 := IFID.io.mux_inst_out(24,20)
ImmGen_mod.io.instr := IFID.io.mux_inst_out
ImmGen_mod.io.pc := IFID.io.pc4_out

// ID_EX pipeline
IDEX.io.IF_ID_pc_in := IFID.io.pc_out
IDEX.io.IF_ID_pc4_in := IFID.io.pc4_out
IDEX.io.fun3_in := IFID.io.mux_inst_out
IDEX.io.func7_in := IFID.io.mux_inst_out
IDEX.io.rs1_in := IFID.io.mux_inst_out
IDEX.io.rs2_in := IFID.io.mux_inst_out
IDEX.io.rd_in := IFID.io.mux_inst_out
IDEX.io.rs1_data_in := RegFile_mod.io.rData1
IDEX.io.rs2_data_in := RegFile_mod.io.rData2
// IDEX.io.ctrl_memWrite_in := Control_mod.io.opcode
// IDEX.io.ctrl_branch_in := Control_mod.io.opcode
// IDEX.io.ctrl_memRead_in := Control_mod.io.opcode
// IDEX.io.ctrl_regWrite_in := Control_mod.io.opcode
// IDEX.io.ctrl_memtoReg_in := Control_mod.io.opcode
// IDEX.io.ctrl_aluOp_in := Control_mod.io.opcode
// IDEX.io.ctrl_OpA_in := Control_mod.io.opcode
// IDEX.io.ctrl_OpB_in := Control_mod.io.opcode
// IDEX.io.ctrl_nextpc_in := Control_mod.io.opcode

ImmGen_mod.io.pc := PC_mod.io.out.asUInt
val a = MuxLookup(Control_mod.io.extend_sel, 0.S, Array(
    (0.U) -> ImmGen_mod.io.I_Type,
    (1.U) -> ImmGen_mod.io.S_Type,
    (2.U) -> ImmGen_mod.io.U_Type
))

//HazardDetection
HazardDetection_mod.io.IF_ID_inst := IFID.io.mux_inst_out
HazardDetection_mod.io.ID_EX_memRead := IDEX.io.ctrl_memRead_out
HazardDetection_mod.io.ID_EX_rd := IDEX.io.rd_out
HazardDetection_mod.io.pc_in := IFID.io.pc4_out
HazardDetection_mod.io.current_pc := IFID.io.pc_out


switch(HazardDetection_mod.io.ctrl_forward){
    is(0.B){
        IDEX.io.ctrl_memWrite_in := Control_mod.io.memwrite
        IDEX.io.ctrl_memRead_in := Control_mod.io.memRead
        IDEX.io.ctrl_branch_in := Control_mod.io.branch
        IDEX.io.ctrl_memtoReg_in := Control_mod.io.memtoReg
        IDEX.io.ctrl_regWrite_in := Control_mod.io.regWrite
        IDEX.io.ctrl_aluOp_in := Control_mod.io.aLUoperation
        IDEX.io.ctrl_OpA_in := Control_mod.io.operand_A_sel
        IDEX.io.ctrl_OpB_in := Control_mod.io.operand_B_sel
        IDEX.io.ctrl_nextpc_in := Control_mod.io.next_pc_sel
    }
    is(1.B){
        IDEX.io.ctrl_memWrite_in := 0.B 
        IDEX.io.ctrl_memRead_in := 0.B
        IDEX.io.ctrl_branch_in := 0.B 
        IDEX.io.ctrl_memtoReg_in := 0.B 
        IDEX.io.ctrl_regWrite_in := 0.B 
        IDEX.io.ctrl_aluOp_in := 0.B 
        IDEX.io.ctrl_OpA_in := 0.B 
        IDEX.io.ctrl_OpB_in := 0.B 
        IDEX.io.ctrl_nextpc_in := 0.B 
    }
}
// JAL
switch(HazardDetection_mod.io.pc_forward){
    is(1.B){
        PC_mod.io.in := HazardDetection_mod.io.pc_out
    }
    is(0.B){
        switch(Control_mod.io.next_pc_sel){
            is("b01".U){
                when(Branch_mod.io.br_taken === 1.U && Control_mod.io.branch === 1.U){
                    PC_mod.io.in := ImmGen_mod.io.UJ_Type
                    IFID.io.pc_in := 0.S 
                    IFID.io.pc4_in := 0.S
                    IFID.io.mux_inst_in := 0.U 
                }.otherwise{
                    PC_mod.io.in := PC4_mod.io.out.asSInt
                }
            }
            is("b10".U){
                    PC_mod.io.in := ImmGen_mod.io.SB_Type
                    IFID.io.pc_in := 0.S 
                    IFID.io.pc4_in := 0.S
                    IFID.io.mux_inst_in := 0.U 
                }
            is("b00".U){
                PC_mod.io.in := PC4_mod.io.out.asSInt
            }
            is("b11".U){
                PC_mod.io.in := PC4_mod.io.out.asSInt
            }
        }
    }   
}

//  Controlling Operand A for ALU
when(IDEX.io.ctrl_OpA_out === "b10".U){
        Alu_mod.io.in_A := IDEX.io.IF_ID_pc4_out
    }
    .otherwise{
        when(ForwardingUnit.io.forward_a === "b00".U){
            Alu_mod.io.in_A := IDEX.io.rs1_data_out
        }
        .elsewhen(ForwardingUnit.io.forward_a === "b01".U){
            Alu_mod.io.in_A := EXMEM.io.EXMEM_alu_out
        }
        .elsewhen(ForwardingUnit.io.forward_a === "b10".U){
            Alu_mod.io.in_A := RegFile_mod.io.write_data
        }
        .otherwise{
            Alu_mod.io.in_A := IDEX.io.rs1_data_out
        }
    }


// JALR
JalR_mod.a := RegFile_mod.io.Reg1
JalR_mod.b := ImmGen_mod.io.instr

//Branch Forwarding

Branchforward_mod.io.ID_EX_RD := IDEX.io.rd_out
Branchforward_mod.io.ID_EX_memRd := IDEX.io.ctrl_memRead_out
Branchforward_mod.io.EX_MEM_RD := EXMEM.io.EXMEM_rd_out
Branchforward_mod.io.EX_MEM_memRd := EXMEM.io.EXMEM_memRd_out
Branchforward_mod.io.MEM_WB_RD := MEMWB.io.MEMWB_rd_out
Branchforward_mod.io.MEM_WB_memRd := MEMWB.io.MEMWB_memRd_out
Branchforward_mod.io.rs1 := IFID.io.mux_inst_out(19,15)
Branchforward_mod.io.rs2 := IFID.io.mux_inst_out(24,20)
Branchforward_mod.io.ctrl_branch := Control_mod.io.branch

//FOR REGISTER 01 IN BRANCH UNIT
switch(Branchforward_mod.io.forward_rs1){
    is("b0000".U){
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b0001".U){
        Branch_mod.io.arg_x := Alu_mod.io.out
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b0010".U){
        Branch_mod.io.arg_x := EXMEM.io.EXMEM_alu_out 
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b0011".U){
        Branch_mod.io.arg_x := RegFile_mod.io.write_data
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b0100".U){
        Branch_mod.io.arg_x := DataMem_mod.io.DataOut
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b0101".U){
        Branch_mod.io.arg_x := RegFile_mod.io.write_data
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b0110".U){
        JalR_mod.a := Alu_mod.io.out
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
    }
    is("b0111".U){
        JalR_mod.a := EXMEM.io.EXMEM_alu_out
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
    }
    is("b1000".U){
        JalR_mod.a := RegFile_mod.io.write_data
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
    }
    is("b1001".U){
        JalR_mod.a := DataMem_mod.io.DataOut
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
    }
    is("b1010".U){
        JalR_mod.a := RegFile_mod.io.write_data
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
    }
    is("b1011".U){
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b1100".U){
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b1101".U){
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b1110".U){
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
        JalR_mod.a := RegFile_mod.io.Reg1
    }
    is("b1111".U){
        Branch_mod.io.arg_x := RegFile_mod.io.Reg1
        JalR_mod.a := RegFile_mod.io.Reg1
    }
}

//FOR REGISTER 02 IN BRANCH UNIT
switch(Branchforward_mod.io.forward_rs2){
    is("b0000".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b0001".U){
        Branch_mod.io.arg_y := Alu_mod.io.out
    }
    is("b0010".U){
        Branch_mod.io.arg_y := EXMEM.io.EXMEM_alu_out 
    }
    is("b0011".U){
        Branch_mod.io.arg_y := RegFile_mod.io.write_data
    }
    is("b0100".U){
        Branch_mod.io.arg_y := DataMem_mod.io.DataOut
    }
    is("b0101".U){
        Branch_mod.io.arg_y := RegFile_mod.io.write_data
    }
    is("b0110".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b0111".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1000".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1001".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1010".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1011".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1100".U){
       Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1101".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1110".U){
       Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
    is("b1111".U){
        Branch_mod.io.arg_y := RegFile_mod.io.Reg2
    }
}
 
//Execute
ForwardingUnit.io.IDEX_rs1 := IDEX.io.rs1_out 
ForwardingUnit.io.IDEX_rs2:= IDEX.io.rs2_out
ForwardingUnit.io.EXMEM_rd := EXMEM.io.EXMEM_rd_out
ForwardingUnit.io.EXMEM_regWr := EXMEM.io.EXMEM_regWrite_out
ForwardingUnit.io.MEMWB_rd := MEMWB.io.MEMWB_rd_out
ForwardingUnit.io.MEMWB_regWr := MEMWB.io.MEMWB_reg_w_out


//EX_MEM pipeline
EXMEM.io.IDEX_MEMRD := IDEX.io.ctrl_memRead_out
EXMEM.io.IDEX_MEMTOREG := IDEX.io.ctrl_memtoReg_out
EXMEM.io.IDEX_MEMWR := IDEX.io.ctrl_memWrite_out
EXMEM.io.IDEX_REG_W := IDEX.io.ctrl_regWrite_out 

when(ForwardingUnit.io.forward_a === "b00".U){
    Alu_mod.io.in_A := IDEX.io.rs1_out 
}.elsewhen(ForwardingUnit.io.forward_a === "b01".U) {
    Alu_mod.io.in_A := EXMEM.io.EXMEM_alu_out
}.elsewhen(ForwardingUnit.io.forward_a === "b10".U) {
    Alu_mod.io.in_A := RegFile_mod.io.write_data
}.otherwise{
    Alu_mod.io.in_A := IDEX.io.rs1_out
}

Control_mod.io.operand_A_sel := IDEX.io.rs2_data_out

Alu_mod.io.in_B := Mux(IDEX.io.ctrl_OpB_out.asBool(), IDEX.io.immm_out, RegFile_mod.io.rData2)
when(IDEX.io.ctrl_OpB_out === "b1".U) {
    Alu_mod.io.in_B := IDEX.io.immm_out

    when(ForwardingUnit.io.forward_b === "b00".U) {
        EXMEM.io.IDEX_rs2 := IDEX.io.rs2_data_out
} 
    .elsewhen(ForwardingUnit.io.forward_b === "b01".U) {
        EXMEM.io.IDEX_rs2 := EXMEM.io.EXMEM_alu_out
}
    .elsewhen(ForwardingUnit.io.forward_b === "b10".U) {
        EXMEM.io.IDEX_rs2 := RegFile_mod.io.write_data
} 
    .otherwise {
        EXMEM.io.IDEX_rs2 := IDEX.io.rs2_out
}
}.otherwise {
    when(ForwardingUnit.io.forward_b === "b00".U) {
        Alu_mod.io.in_B := IDEX.io.rs2_out
        EXMEM.io.IDEX_rs2 := IDEX.io.rs2_out
    
 } 
    .elsewhen(ForwardingUnit.io.forward_b === "b01".U) {
        Alu_mod.io.in_B := EXMEM.io.EXMEM_alu_out
        EXMEM.io.IDEX_rs2 := EXMEM.io.EXMEM_alu_out
 } 
    .elsewhen(ForwardingUnit.io.forward_b === "b10".U) {
        Alu_mod.io.in_B := RegFile_mod.io.write_data
        EXMEM.io.IDEX_rs2 := RegFile_mod.io.write_data
 } 
    .otherwise {
        Alu_mod.io.in_B := IDEX.io.rs2_out
        EXMEM.io.IDEX_rs2:= IDEX.io.rs2_out
 }
}

EXMEM.io.alu_in := Alu_mod.io.out
AluControl_mod.io.alu_op := IDEX.io.ctrl_aluOp_out
AluControl_mod.io.func3 := IDEX.io.fun3_out
AluControl_mod.io.func7 := IDEX.io.func7_out
EXMEM.io.IDEX_rs2 := IDEX.io.rs2_out 
EXMEM.io.IDEX_rd := IDEX.io.rd_out

//Memory
DataMem_mod.io.mem_read := EXMEM.io.EXMEM_memRd_out
DataMem_mod.io.mem_write := EXMEM.io.EXMEM_memWr_out
MEMWB.io.EXMEM_MEMTOREG := EXMEM.io.EXMEM_memToReg_out
MEMWB.io.EXMEM_REG_W := EXMEM.io.EXMEM_regWrite_out
MEMWB.io.EXMEM_rd := EXMEM.io.EXMEM_rd_out
DataMem_mod.io.Addr := EXMEM.io.EXMEM_alu_out
DataMem_mod.io.DataIn := EXMEM.io.EXMEM_rs2_out

RegFile_mod.io.write_Reg := MEMWB.io.MEMWB_rd_out
RegFile_mod.io.Reg_write := MEMWB.io.MEMWB_reg_w_out

//MEM_WB Pipeline
MEMWB.io.in_dataMem := DataMem_mod.io.DataOut
MEMWB.io.MEMWB_alu_out := DataMem_mod.io.Addr 

//WriteBack
val h = Mux(MEMWB.io.MEMWB_memToReg_out, MEMWB.io.MEMWB_dataMem_out, MEMWB.io.MEMWB_alu_out)

RegFile_mod.io.write_data := h 
io.out := 0.S 

}