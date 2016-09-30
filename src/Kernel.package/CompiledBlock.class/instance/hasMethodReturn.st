scanning
hasMethodReturn
	"Answer whether the receiver has a method-return ('^') in its code."
	^ (InstructionStream on: self) scanFor: [:byte | 
		 self encoderClass methodReturnBytecodes includes: byte]