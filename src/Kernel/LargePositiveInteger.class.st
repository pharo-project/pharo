"
I represent positive integers of more than 30 bits (ie, >= 1073741824).  These values are beyond the range of SmallInteger, and are encoded here as an array of 8-bit digits.  Care must be taken, when new values are computed, that any result that COULD BE a SmallInteger IS a SmallInteger (see normalize).

Note that the bit manipulation primitives, bitAnd:, bitShift:, etc., = and ~= run without failure (and therefore fast) if the value fits in 32 bits.  This is a great help to the simulator.
"
Class {
	#name : #LargePositiveInteger,
	#superclass : #LargeInteger,
	#type : #bytes,
	#category : #'Kernel-Numbers'
}

{ #category : #arithmetic }
LargePositiveInteger >> abs [
]

{ #category : #'bit manipulation' }
LargePositiveInteger >> bitAt: anInteger [
	"Optimize super algorithm to avoid long bit operations.
	Instead work on digits which are known to be SmallInteger and fast.
	Note that this algorithm does not work for negative integers."
	
	| digitIndex bitIndex |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self bytesCount ifTrue: [^0].
	bitIndex := anInteger - 1 \\ 8 + 1.
	^(self byteAt: digitIndex) bitAt: bitIndex
]

{ #category : #'bit manipulation' }
LargePositiveInteger >> hashMultiply [
	"This is a multiplication of hashes by 1664525 mod 2^28 written to avoid overflowing into large integers.
	 The primitive is able to perform the operation with modulo arihmetic.
	
	Example of usage:
	  hash
	    ^ (super hash + variableName hash) hashMultiply 	
	"
	
	<primitive: 159>
	"Truncate to 28 bits and try again"
	^(self bitAnd: 16rFFFFFFF) hashMultiply
]

{ #category : #'bit manipulation' }
LargePositiveInteger >> highBit [
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."
	^ self highBitOfMagnitude
]

{ #category : #testing }
LargePositiveInteger >> mightBeASquare [
	"In base 16, a square number can end only with 0,1,4 or 9 and
	- in case 0, only 0,1,4,9 can precede it,
	- in case 4, only even numbers can precede it.
	See http://en.wikipedia.org/wiki/Square_number
	So, in hex, the last byte must be one of:
		00
		10
		40
		90
		x1
		e4
		x9
	where x is any hex digit and e is any even digit
	Also, the receiver must be an aven power of two."
	| lsb |
	lsb := self byteAt: 1.
	^(lsb = 0 and: [ self lowBit odd ])	"00 (and even power of 2)"
		or: [ lsb = 16r40				"40"
		or: [ (lsb bitAnd: 16r7) = 1		"any|1 or any|9"
		or: [ (lsb bitAnd: 16r1F) = 4		"even|4"
		or: [ (lsb bitAnd: 16r7F) = 16 ]]]]	"10 or 90"
]

{ #category : #arithmetic }
LargePositiveInteger >> negated [ 
	^ (self copyto: (LargeNegativeInteger new: self bytesCount))
		normalize  "Need to normalize to catch SmallInteger minVal"
]

{ #category : #testing }
LargePositiveInteger >> negative [
	"Answer whether the receiver is mathematically negative."

	^ false
]

{ #category : #converting }
LargePositiveInteger >> normalize [
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen |
	<primitive: 'primNormalizePositive' module:'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self bytesCount.
	[len = 0 ifTrue: [^0].
	(self byteAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.  Fast compute SmallInteger maxVal digitLength"
	sLen := SmallInteger maxVal > 16r3FFFFFFF
				ifTrue: [8]
				ifFalse: [4].
	(len <= sLen
	 and: [(self byteAt: sLen) <= (SmallInteger maxVal byteAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val := 0.
				len to: 1 by: -1 do:
					[:i | val := (val *256) + (self byteAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]
]

{ #category : #testing }
LargePositiveInteger >> positive [
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ true
]

{ #category : #printing }
LargePositiveInteger >> printOn: aStream base: b [
	"Append a representation of this number in base b on aStream.
	In order to reduce cost of LargePositiveInteger ops, split the number in approximately two equal parts in number of digits."
	
	| halfDigits halfPower head tail nDigitsUnderestimate |
	"Don't engage any arithmetic if not normalized"
	(self bytesCount = 0 or: [(self byteAt: self bytesCount) = 0]) 
		ifTrue: [ ^self normalize printOn: aStream base: b ].
	
	nDigitsUnderestimate := b = 10
		ifTrue: [((self highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [self highBit quo: b highBit].
		
	"splitting digits with a whole power of two is more efficient"
	halfDigits := 1 bitShift: nDigitsUnderestimate highBit - 2.
	
	halfDigits <= 1
		ifTrue: ["Hmmm, this could happen only in case of a huge base b... Let lower level fail"
			^self printOn: aStream base: b nDigits: (self numberOfDigitsInBase: b)].
	
	"Separate in two halves, head and tail"
	halfPower := b raisedToInteger: halfDigits.
	head := self quo: halfPower.
	tail := self - (head * halfPower).
	
	"print head"
	head printOn: aStream base: b.
	
	"print tail without the overhead to count the digits"
	tail printOn: aStream base: b nDigits: halfDigits
]

{ #category : #testing }
LargePositiveInteger >> sign [
	"Optimization. Answer 1 since receiver is greater than 0."

	^ 1

]

{ #category : #testing }
LargePositiveInteger >> signBit [
	"Optimization."

	^0
]

{ #category : #testing }
LargePositiveInteger >> strictlyPositive [
	"Answer whether the receiver is mathematically positive."

	^ true
]
