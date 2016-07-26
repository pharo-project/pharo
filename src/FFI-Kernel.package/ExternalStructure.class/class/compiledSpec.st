field definition
compiledSpec
	"Return the compiled spec of the receiver"
	^compiledSpec ifNil:[self compileFields].