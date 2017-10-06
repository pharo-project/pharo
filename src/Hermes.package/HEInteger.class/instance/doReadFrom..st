reading
doReadFrom: aReader
	| anArray isNegative |
	isNegative := aReader readByte = 1.
	anArray := aReader readByteArray.

	value := anArray asInteger *(isNegative ifTrue: -1 ifFalse: 1)