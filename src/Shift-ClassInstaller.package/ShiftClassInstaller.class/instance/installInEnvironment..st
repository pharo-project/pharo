building
installInEnvironment: newClass
	"I only install in the environment if there is not oldClass installed."
	(builder environment includesKey: builder name) ifFalse:[
		builder environment at: builder name put: newClass
	].

	self
		updateBindings: (builder environment associationAt: builder name)
		of: newClass