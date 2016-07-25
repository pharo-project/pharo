mathematical functions
lcm: n 
	"Answer the least common multiple of the receiver and n."

	^self // (self gcd: n) * n