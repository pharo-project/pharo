nonhomogeneous accessing
nextWord
	"Answer the next two bytes from the receiver as an Integer."

	| high low |
	high := self next.
		high==nil ifTrue: [^false].
	low := self next.
		low==nil ifTrue: [^false].
	^(high asInteger bitShift: 8) + low asInteger