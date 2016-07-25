gc
setGCSemaphore: semaIndex
	"Primitive. Indicate the GC semaphore index to be signaled on GC occurance."
	<primitive: 'primitiveSetGCSemaphore'>
	^self primitiveFailed
"Example:

	| index sema process |
	sema := Semaphore new.
	index := Smalltalk registerExternalObject: sema.
	Smalltalk setGCSemaphore: index.
	process := [
		[[true] whileTrue:[
			sema wait.
			Smalltalk beep.
		]] ensure:[
			Smalltalk setGCSemaphore: 0.
			Smalltalk unregisterExternalObject: sema.
		].
	] fork.
	process inspect.
"