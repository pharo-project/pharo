*Tool-Profilers
profileSemaphore: aSemaphore
	"Primitive. Install the semaphore to be used for profiling, 
	or nil if no semaphore should be used.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSemaphore'>
	^self primitiveFailed