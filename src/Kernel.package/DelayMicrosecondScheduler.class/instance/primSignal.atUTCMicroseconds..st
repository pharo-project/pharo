primitives
primSignal: aSemaphore atUTCMicroseconds: aLargePositiveInteger
	"Signal the semaphore when the microsecondsecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 242>
	^ self primitiveFailed