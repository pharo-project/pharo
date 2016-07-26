mathematical functions
floorLog: radix
	"Answer the floor of the log base radix of the receiver.
	The result may be off by one due to rounding errors, except in base 2."

	(radix = 2 and: [self > 0.0]) ifTrue: [^self exponent].
	^ (self log: radix) floor
