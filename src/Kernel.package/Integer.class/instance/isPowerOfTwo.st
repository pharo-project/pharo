testing
isPowerOfTwo
	"Return true if the receiver is an integral power of two."
	^ self ~= 0 and: [(self bitAnd: self-1) = 0]