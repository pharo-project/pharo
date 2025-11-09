Extension { #name : 'InstructionStream' }

{ #category : '*OpalCompiler-Core' }
InstructionStream >> nextPc: currentByte [
	"Answer the pc of the next bytecode following the current one, given the current bytecode.
	 Skips the extension bytecodes."
	^ pc + (self method encoderClass instructionSizeAt: pc of: self method)
]
