// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for 64-bit R-type
 *                          . 2 for 64-bit I-type
 *                          . 3 for 32-bit R-type
 *                          . 4 for 32-bit I-type
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  when (io.aluop === 1.U) { // 64-bit R-type
    io.operation := MuxCase(
      "b11111".U, // default: Invalid
      Array(
        ((io.funct3 === "b111".U) & (io.funct7 === "b0000000".U)) -> "b01101".U, // and
        ((io.funct3 === "b000".U) & (io.funct7 === "b0100000".U)) -> "b00010".U, // sub
        ((io.funct3 === "b000".U) & (io.funct7 === "b0000001".U)) -> "b00101".U, // mul
        ((io.funct3 === "b001".U) & (io.funct7 === "b0000000".U)) -> "b10010".U, // sll
        ((io.funct3 === "b001".U) & (io.funct7 === "b0000001".U)) -> "b00111".U, // mulh
        ((io.funct3 === "b010".U) & (io.funct7 === "b0000000".U)) -> "b10110".U, // slt
        ((io.funct3 === "b010".U) & (io.funct7 === "b0000001".U)) -> "b11000".U, // mulhsu
        ((io.funct3 === "b011".U) & (io.funct7 === "b0000000".U)) -> "b10111".U, // sltu
        ((io.funct3 === "b011".U) & (io.funct7 === "b0000001".U)) -> "b01000".U, // mulhu
        ((io.funct3 === "b100".U) & (io.funct7 === "b0000000".U)) -> "b01111".U, // xor
        ((io.funct3 === "b100".U) & (io.funct7 === "b0000001".U)) -> "b01001".U, // div
        ((io.funct3 === "b101".U) & (io.funct7 === "b0000000".U)) -> "b10100".U, // srl
        ((io.funct3 === "b101".U) & (io.funct7 === "b0100000".U)) -> "b10000".U, // sra
        ((io.funct3 === "b101".U) & (io.funct7 === "b0000001".U)) -> "b01010".U, // divu
        ((io.funct3 === "b110".U) & (io.funct7 === "b0000000".U)) -> "b01110".U, // or
        ((io.funct3 === "b110".U) & (io.funct7 === "b0000001".U)) -> "b11100".U, // rem
        ((io.funct3 === "b000".U) & (io.funct7 === "b0000000".U)) -> "b00000".U, // add
        ((io.funct3 === "b111".U) & (io.funct7 === "b0000001".U)) -> "b11011".U  // remu
      )
    )
  }
  .elsewhen (io.aluop === 3.U) { // 32-bit R-type
    io.operation := MuxCase(
      "b11111".U, // default: Invalid
      Array(
        ((io.funct3 === "b000".U) & (io.funct7 === "b0000000".U)) -> "b00001".U, // addw
        ((io.funct3 === "b000".U) & (io.funct7 === "b0100000".U)) -> "b00100".U, // subw
        ((io.funct3 === "b000".U) & (io.funct7 === "b0000001".U)) -> "b00110".U, // mulw
        ((io.funct3 === "b001".U) & (io.funct7 === "b0000000".U)) -> "b10011".U, // sllw
        ((io.funct3 === "b100".U) & (io.funct7 === "b0000001".U)) -> "b01011".U, // divw
        ((io.funct3 === "b101".U) & (io.funct7 === "b0000000".U)) -> "b10101".U, // srlw
        ((io.funct3 === "b101".U) & (io.funct7 === "b0100000".U)) -> "b10001".U, // sraw
        ((io.funct3 === "b101".U) & (io.funct7 === "b0000001".U)) -> "b01100".U, // divuw
        ((io.funct3 === "b110".U) & (io.funct7 === "b0000001".U)) -> "b11010".U, // remw
        ((io.funct3 === "b111".U) & (io.funct7 === "b0000001".U)) -> "b11001".U  // remuw
      )
    )
  }

}
