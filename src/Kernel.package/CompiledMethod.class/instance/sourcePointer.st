source code management
sourcePointer
	"Answer the integer which can be used to find the source file and position for this method.
	The actual interpretation of this number is up to the SourceFileArray stored in the global variable SourceFiles."

	^ self trailer sourcePointer
