running
bootstrapInitialProcessCode
	| instructions |
	instructions := (self class>>#bootstrapInitialProcess) sourceCode.
	^ instructions allButFirst: (instructions indexOf: Character cr) "remove method name"