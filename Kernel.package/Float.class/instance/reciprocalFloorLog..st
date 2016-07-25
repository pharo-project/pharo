mathematical functions
reciprocalFloorLog: radix 
	"Quick computation of (self log: radix) floor, when self < 1.0.
	Avoids infinite recursion problems with denormalized numbers"

	| adjust scale n |
	adjust := 0.
	scale := 1.0.
	[(n := radix / (self * scale)) isInfinite]
		whileTrue:
			[scale := scale * radix.
			adjust := adjust + 1].
	^ ((n floorLog: radix) + adjust) negated