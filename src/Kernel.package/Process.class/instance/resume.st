changing process state
resume
	"Allow the process that the receiver represents to continue. Put  
	the receiver in line to become the activeProcess. Check for a nil 
	suspendedContext, which indicates a previously terminated Process that 
	would cause a vm crash if the resume attempt were permitted"

	suspendedContext ifNil: [^ self primitiveFailed].
	^ self primitiveResume