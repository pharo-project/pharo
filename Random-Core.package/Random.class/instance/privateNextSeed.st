private
privateNextSeed
	"This method generates random instances of Integer 	in the interval
	0 to 16r7FFFFFFF. This method does NOT update the seed; repeated sends
	answer the same value. The algorithm is described in detail in [1]"

	| lo hi aLoRHi |
	hi := (seed quo: q) asFloat.
	lo := seed - (hi * q).  " = seed rem: q"  
	aLoRHi := (a * lo) - (r * hi).
	^(aLoRHi > 0.0)
		ifTrue:  [aLoRHi]
		ifFalse: [aLoRHi + m].