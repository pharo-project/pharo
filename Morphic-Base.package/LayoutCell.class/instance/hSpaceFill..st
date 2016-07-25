accessing
hSpaceFill: aBool
	flags := aBool ifTrue:[self flags bitOr: 1] ifFalse:[self flags bitClear: 1].
