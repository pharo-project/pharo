*UnifiedFFI
ffiInstVarArgument: argName generator: generator
	"Load the instance variable with given name" 

	(self allInstVarNames includes: argName) ifFalse: [ ^ nil ].
	^ FFIInstVarArgument new 
		argName: argName;
		yourself