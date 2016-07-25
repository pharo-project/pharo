accessing
recycle: byteArray
	"Return byteArray, which is no longer needed, to the receiver to be recyled 
	and reused when #byteArrayOfSize:zero: is called."
	
	limit = 0 ifTrue: [ ^ self ].
	access critical: [ 
		| pool |
		pool := self poolForSize: byteArray size.
		"Find an empty slot in the pool"
		pool withIndexDo: [ :value :index | 
			value ifNil: [ 
				^ pool at: index put: byteArray ] ]
		"If the pool is full, don't hold onto it" ]