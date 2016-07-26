bit manipulation
bitShift: shiftCount 
	"Answer an Integer whose value (in twos-complement representation) is  
	the receiver's value (in twos-complement representation) shifted left by 
	the number of bits indicated by the argument. Negative arguments  
	shift right. Zeros are shifted in from the right in left shifts."
	| magnitudeShift |
	magnitudeShift := self bitShiftMagnitude: shiftCount.
	^ ((self negative and: [shiftCount negative])
		and: [self anyBitOfMagnitudeFrom: 1 to: shiftCount negated])
		ifTrue: [magnitudeShift - 1]
		ifFalse: [magnitudeShift]