libgit-calls
libgit2_featuresAlt
	"Query compile time options for libgit2."
	
	| fnc |
	
	fnc := ExternalLibraryFunction
		name: 'git_libgit2_features'
		module: 'libgit2.0.dylib'
		callType: 0
		returnType: FFIInt32 new externalTypeWithArity
		argumentTypes: {}.
		
	^fnc invokeWithArguments: { }
