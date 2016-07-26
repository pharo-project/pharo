testing
willJumpIfFalse
	"Answer whether the next bytecode is a jump-if-false."

	^ self method encoderClass isBranchIfFalseAt: pc in: self method