setting
maximumButtons: anInteger
	(anInteger isKindOf: Integer) ifFalse: [ ^ self ].
	anInteger > 0 ifFalse: [ ^ self ].
	MaximumButtons := anInteger.