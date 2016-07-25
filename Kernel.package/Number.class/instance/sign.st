mathematical functions
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0