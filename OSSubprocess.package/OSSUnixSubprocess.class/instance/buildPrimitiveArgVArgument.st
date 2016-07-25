argv building
buildPrimitiveArgVArgument
	"Takes care of allocating and building the array of arguments as expected
	by the posix_spawn() function. 
	Answers the array ready to be used."
	| bufferPointer |
	bufferPointer := ExternalAddress allocate: self argVArguments size * self systemAccessor sizeOfPointer.
	self registerPointer: bufferPointer.
	self collectArgumentPointersInto: bufferPointer.
	 ^ bufferPointer