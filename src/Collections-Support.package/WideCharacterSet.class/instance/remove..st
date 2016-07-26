collection ops
remove: aCharacter 
	| val high low lowmap |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [self byteArrayMap at: val + 1 put: 0].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := map
				at: high
				ifAbsent: [^ aCharacter].
	self clearBitmap: lowmap at: low.
	(lowmap allSatisfy: [:e | e = 0])
		ifTrue: [map removeKey: high].
	^ aCharacter