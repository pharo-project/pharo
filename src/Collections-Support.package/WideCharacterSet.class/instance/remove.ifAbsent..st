removing
remove: aCharacter ifAbsent: aBlock
	| val high low lowmap |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [
		| index |
		index := val + 1.
		((byteArrayMap at: index) = 0) ifTrue: [ ^ aBlock value ].
		byteArrayMap at: index put: 0.
		^ aCharacter
	].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := map
				at: high
				ifAbsent: [^ aBlock value].
	self clearBitmap: lowmap at: low ifNotSet: [^ aBlock value].
	(lowmap allSatisfy: [:e | e = 0])
		ifTrue: [map removeKey: high].
	^ aCharacter