comparing
hash
	"Hash is reimplemented because = is implemented. Both words of the float are used; 8 bits are removed from each end to clear most of the exponent regardless of the byte ordering. (The bitAnd:'s ensure that the intermediate results do not become a large integer.) Slower than the original version in the ratios 12:5 to 2:1 depending on values. (DNS, 11 May, 1997)"

	(self isFinite and: [self fractionPart = 0.0]) ifTrue: [^self truncated hash].
	^ (((self basicAt: 1) bitAnd: 16r00FFFF00) +
	   ((self basicAt: 2) bitAnd: 16r00FFFF00)) bitShift: -8
