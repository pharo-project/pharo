I scan compiled method to detect loops. I answer an array or the size of the compiled method I scanned, which holds a FBDLoop at the pc where a loop starts and nil for any other pc. The FBDLoop describes the loop.

instructionStream <InstructionStream> allows me to decode the method's bytecode
branchTargets <Array of (Smi | nil)> at any pc where a conditional jump goes, I put an annotation in this array. This is used to know if a loop is conditional or unconditional.
loops <Array of (FBDLoop | nil)> array answered by the scanner. it holds a FBDLoop at the pc where a loop starts and nil for any other pc.
currentPC <Smi> by opposition to self pc which holds the pc just *after* the instruction being decoded, currentPC holds the pc of the instruction being decoded. This is useful as you can't read the bytecode backward due to multiple bytecodes instructions.
