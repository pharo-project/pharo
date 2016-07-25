private
bitmap: aMap at: shortInteger
	"access a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"
	
	| collecIndex bitIndex |
	collecIndex := shortInteger bitShift: -3.
	bitIndex := shortInteger bitAnd: 7.
	^(aMap at: collecIndex + 1) bitAnd: (1 bitShift: bitIndex)