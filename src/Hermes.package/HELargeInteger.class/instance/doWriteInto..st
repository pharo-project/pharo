writing
doWriteInto: aWriter
	| anArray |
	aWriter writeByte: (value sign < 0 ifTrue: [ 1 ]	ifFalse: [ 0 ]).
	anArray := ByteArray new: value size.
	1 to:value size do:[:idx | anArray at:idx put:(value at:idx)].
	aWriter writeByteArray: anArray.