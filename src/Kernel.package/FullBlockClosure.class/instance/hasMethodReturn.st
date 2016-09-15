testing
hasMethodReturn
	"Answer whether the receiver has a method-return ('^') in its code."
	^ (InstructionStream on: self compiledBlock) 
		scanFor: [:byte | 
			self compiledBlock encoderClass methodReturnBytecodes includes: byte]