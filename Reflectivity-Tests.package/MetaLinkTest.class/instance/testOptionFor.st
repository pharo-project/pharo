tests - options
testOptionFor

	self assert: (((ReflectivityExamples>>#exampleMethodWithMetaLinkOptions) ast optionsFor: MetaLink new) includes: #optionCompileOnLinkInstallation).
	self assert: (((ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) ast optionsFor: MetaLink new) includes: #optionCompileOnLinkInstallation).