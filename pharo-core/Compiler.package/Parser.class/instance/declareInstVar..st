error correction
declareInstVar: name
	"Declare an instance variable.  Since the variable will get added after any existing
	 inst vars its index is the instSize."
	encoder classEncoding addInstVarNamed: name.
	^InstanceVariableNode new name: name index: encoder classEncoding instSize
		