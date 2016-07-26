instruction stream support
isExtension: bytecode
	"Answer if the bytecode is an extension bytecode, i.e. one that extends
	 the range of the following bytecode."
	^bytecode >= 16rE0 and: [bytecode <= 16rE1]