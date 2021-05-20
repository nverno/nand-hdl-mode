// -*- mode: hack-assembler-*-
// n = 0
// addr = 100
// do {
//     RAM[addr] = n
//     addr = addr+1
//     n = n+11
// } while n <= 99
   @n                                             // n = 0
   M=0

   @100                                           // addr = 100
   D=A
   @addr
   M=D

(Loop)                                            // do {
   @n                                             //   RAM[addr] = n
   D=M
   @addr
   A=M
   M=D

   @addr                                          // addr = addr + 1
   M=M+1

   @11                                            // n = n + 11
   D=A
   @n
   MD=D+M                                         // n + 11 also in D

   @99                                            // while n <= 99
   D=D-A
   @Loop
   D;JLE

   @Halt                                          // Loop forever
(Halt)
   0; JMP
