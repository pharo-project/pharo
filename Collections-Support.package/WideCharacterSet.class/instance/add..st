collection ops
add: aCharacter 
	| val high low lowmap |
	val := aCharacter asciiValue.
	val < 256 ifTrue: [self byteArrayMap at: val + 1 put: 1].
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := map at: high ifAbsentPut: ["create a chunk of 65536=8192*8 bits"
		ByteArray new: 8192].
	self setBitmap: lowmap at: low.
	^ aCharacter