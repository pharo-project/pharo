bit manipulation
allMask: mask 
	"Treat the argument as a bit mask. Answer whether all of the bits that 
	are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask)