accessing
parseAnonymousFunction: aFunctionDefinition
	"Parse a function definition in a form of: 
	
	 #( returnType (arguments) )
	
	 or

	 'returnType (arguments)'

	 Answer an instance of FFIFunctionSpec class"
	| argsSpec retType |
	
	fnSpec := FFIFunctionSpec new.
	
	aFunctionDefinition isString 
		ifTrue: [
			retType :=  (aFunctionDefinition copyUpTo: $( ) trimBoth.
			argsSpec := ((aFunctionDefinition copyAfter: $( ) copyUpTo: $)) trimBoth ] 
		ifFalse: [
			argsSpec := aFunctionDefinition last flatCollect: [:ea | ea asString, ' ' ].
			retType := aFunctionDefinition allButLast  flatCollect: [:ea | ea asString, ' ' ] ].
	
	"Parse return type"
	stream := retType readStream.
	fnSpec returnType: self parseReturn.

	stream atEnd ifFalse: [ self error: 'Nothing more expected after function type'].	
	
	"Function arguments"
	stream := argsSpec readStream.
	self parseArguments.

	^ fnSpec