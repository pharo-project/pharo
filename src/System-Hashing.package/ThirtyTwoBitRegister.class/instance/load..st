accessing
load: anInteger
	"Set my contents to the value of given integer."

	low := anInteger bitAnd: 16rFFFF.
	hi := (anInteger bitShift: -16) bitAnd: 16rFFFF.
	self asInteger = anInteger
		ifFalse: [self error: 'out of range: ', anInteger printString].
