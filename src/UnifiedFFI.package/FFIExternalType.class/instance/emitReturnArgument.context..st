emitting code
emitReturnArgument: builder context: aContext
	"Some times functions need some post-process (for example, to unpack pointers). 
	 This call MUST exit with result value in top of the stack (otherwise it will 
	interfere with emitReturn:resultTempVar:context:"
	self loader 
		emitPointerArityUnpack: builder 
		type: self
		context: aContext