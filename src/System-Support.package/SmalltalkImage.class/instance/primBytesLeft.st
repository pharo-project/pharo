memory space
primBytesLeft
	"Primitive. Answer the number of free bytes available in old space.
	 Not accurate unless preceded by
		Smalltalk garbageCollectMost (for reasonable accuracy), or
		Smalltalk garbageCollect (for real accuracy).
	 See Object documentation whatIsAPrimitive."

	<primitive: 112>
	^0