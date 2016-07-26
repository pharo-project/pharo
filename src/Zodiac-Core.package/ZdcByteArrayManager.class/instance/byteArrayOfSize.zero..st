accessing
byteArrayOfSize: size zero: zero
	"Return a ByteArray of size. If zero is true, make sure all bytes equal zero.
	When the byte array is no longer needed, #recycle can be used to return it to the reciever.
	It is possible that the byte array returned was previously recycled."
	
	limit = 0 ifTrue: [ ^ ByteArray new: size ].
	access critical: [ 
		| pool |
		pool := self poolForSize: size.
		"Look for an instance to reuse without allocation"
		pool withIndexDo: [ :value :index | 
			value ifNotNil: [ 
				pool at: index put: nil.
				zero ifTrue: [ value atAllPut: 0 ].
				^ value ] ].
		"Allocate a new instance "
		^ ByteArray new: size ]