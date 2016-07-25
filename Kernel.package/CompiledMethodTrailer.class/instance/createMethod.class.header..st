creating a method
createMethod: numberOfBytesForAllButTrailer class: aCompiledMethodClass header: headerWord 
	| meth |
	"Instantiates a compiled method with this trailer.
	- numberOfBytesForAllButTrailer specifies the number of bytes needed to encode the literals and the bytecode.
	- aCompiledMethodClass specifies the concrete class of the compiled method.
	- headerWord specifies the header used for the compiled method.
	
	This method will create a compiled method instance of the correct size, set it's header and trailing bytes. The bytecode and literals must be set by the caller."
	
	encodedData ifNil: [ self encode ].
	
	meth := aCompiledMethodClass newMethod: numberOfBytesForAllButTrailer + size header: headerWord.
	"copy the encoded trailer data"
	1 to: size do:
		[:i | meth at: meth size - size + i put: (encodedData at: i)].

	^meth