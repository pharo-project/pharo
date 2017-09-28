building
installInEnvironment: newClass
	builder environment at: builder name put: newClass.
	self
		updateBindings: (builder environment associationAt: builder name)
		of: newClass