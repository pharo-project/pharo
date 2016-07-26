accessing
numTemps
	"Answer the number of temporary variables used by the receiver."
	
	^ (self header bitShift: -18) bitAnd: 16r3F