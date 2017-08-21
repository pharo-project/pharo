writing
doReadFrom: aReader
	| anArray isNegative |
	isNegative := aReader readByte = 1.
	anArray := aReader readByteArray.

	value := isNegative
		ifTrue: [ LargeNegativeInteger new: anArray size ]
		ifFalse: [ LargePositiveInteger new: anArray size ].

	1 to: anArray size do: [ :idx | value at: idx put: (anArray at: idx) ].
