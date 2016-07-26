primitives
primSignal: aSemaphore atMilliseconds: aSmallInteger
	"Signal the semaphore when the millisecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 136>
	^self primitiveFailed