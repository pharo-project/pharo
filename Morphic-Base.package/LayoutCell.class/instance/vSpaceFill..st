accessing
vSpaceFill: aBool
	flags := aBool ifTrue:[self flags bitOr: 2] ifFalse:[self flags bitClear: 2].
