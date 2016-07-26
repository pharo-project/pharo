bit manipulation
anyMask: mask 
	"Treat the argument as a bit mask. Answer whether any of the bits that 
	are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask)