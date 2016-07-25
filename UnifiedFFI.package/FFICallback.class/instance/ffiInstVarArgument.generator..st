private
ffiInstVarArgument: argName generator: generator
	"Load the instance variable with given name. 
	 It will be ignored anyway in a callback" 
	^ FFIInstVarArgument new 
		argName: argName;
		yourself