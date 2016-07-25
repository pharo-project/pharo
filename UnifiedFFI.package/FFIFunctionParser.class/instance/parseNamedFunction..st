accessing
parseNamedFunction: aFunctionDefinition
	"Parse a function definition in a form of: 

	 #( returnType functionName (arguments) )
	 
	 or

	 'returnType functionName (arguments)'

	 Answer an instance of FFIFunctionSpec class"
	| argsSpec typeAndName fnName |
	
	fnSpec := FFIFunctionSpec new.
	
	aFunctionDefinition isString 
		ifTrue: [
			typeAndName :=  (aFunctionDefinition copyUpTo: $( ) trimBoth.
			argsSpec := ((aFunctionDefinition copyAfter: $( ) copyUpTo: $)) trimBoth ] 
		ifFalse: [
			argsSpec := aFunctionDefinition last flatCollect: [:ea | ea asString, ' ' ].
			typeAndName := aFunctionDefinition allButLast flatCollect: [:ea | ea asString, ' ' ] ].
		
	"Parse return type"
	stream := typeAndName readStream.
	fnSpec returnType: self parseReturn.

	"Function name"	
	fnName := self parseWord.
	fnName ifNil: [ self error: 'function name expected' ].
	fnSpec functionName: fnName.

	"Function arguments"	
	stream := argsSpec readStream.
	self parseArguments.

	^ fnSpec