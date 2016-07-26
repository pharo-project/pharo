testing
willJumpIfTrue 
	"Answer whether the next bytecode is a jump-if-true."
 
	^ self method encoderClass isBranchIfTrueAt: pc in: self method