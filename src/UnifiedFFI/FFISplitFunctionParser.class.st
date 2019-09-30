Class {
	#name : #FFISplitFunctionParser,
	#superclass : #FFIFunctionParser,
	#category : #'UnifiedFFI-Base'
}

{ #category : #accessing }
FFISplitFunctionParser >> parseArgument [
	| argument argName indirectIndex typeAndPtrArity next |
	" An argument can be:

 - nil , true , false
 - a class/pool variable name
 - integer literal

 - self , with optional **
 - type name *** arg name

"
	"constant conditions"
	(stream contents size = 1 and: [ 
		stream contents first ~= #void ]) ifTrue: [ 
		^ self requestor integerConstantArgument: stream next ].
	stream peek isArray ifTrue: [ 
		^ self parseArgumentCastedType ].

	"none of them, parse type and name"
	typeAndPtrArity := self parseType.
	argName := stream next.
	next := stream peek.
	next = $@
		ifTrue: [ 
			stream next.
			argName := argName.
			indirectIndex := self parseInt ].	
	"for sole 'void' fake argument "
	(typeAndPtrArity = #('void' 0) and: [ argName isNil ])
		ifTrue: [ ^ nil ].
	argument := requestor
		argName: argName
		indirectIndex: indirectIndex
		type: typeAndPtrArity first
		ptrArity: typeAndPtrArity second.

	argument resolvedType isExternalType
		ifTrue: [ argument resolvedType validateAfterParse: typeAndPtrArity ].

	^ argument 
]

{ #category : #accessing }
FFISplitFunctionParser >> parseArgumentCastedType [
	| castedType number |

	castedType := self parseCastedType.
	number := stream next.
	^ self requestor integerConstantArgument: number type: castedType
]

{ #category : #accessing }
FFISplitFunctionParser >> parseArguments [
"
Parse a arguments spec in a form of: 

#( type1 name1 , type2 name2 , nil, true, false , 100 )
"
	| allThings |
	allThings := stream.
	[ allThings atEnd ] whileFalse: [
		stream := (allThings upTo: #,) readStream.
		self parseArgument ifNotNil: [ :arg | 
			fnSpec addArgument: arg ] ]
]

{ #category : #accessing }
FFISplitFunctionParser >> parseCastedType [

	^ (String streamContents: [ :castedType |
		stream next do: [ :typepart |
			castedType
				nextPutAll: typepart;
				space ] ]) trimBoth
]

{ #category : #accessing }
FFISplitFunctionParser >> parseNamedFunction: aFunctionDefinition [
	"Parse a function definition in a form of: 

	 #( returnType functionName (arguments) )
	 
	 or

	 'returnType functionName (arguments)'

	 Answer an instance of FFIFunctionSpec class"
	| argsSpec typeAndName fnName |
	
	fnSpec := FFIFunctionSpec new.
	
	self setOn: aFunctionDefinition.
	
	argsSpec := stream contents last.
	typeAndName := stream contents allButLast.
		
	"Parse return type"
	stream := typeAndName readStream.
	fnSpec returnType: self parseReturn.

	"Function name"	
	fnName := stream next.
	fnName ifNil: [ self error: 'function name expected' ].
	fnSpec functionName: fnName.

	"Function arguments"	
	stream := argsSpec readStream.
	self parseArguments.

	^ fnSpec
]

{ #category : #accessing }
FFISplitFunctionParser >> parseType [

	" parse type name and optional number of asterisks, following it"

	| typeName ptrArity |

	typeName := stream next.
	typeName ifNil: [ ^ self error: 'type name expected' ].	

	"skip 'const' , which is often used but has no any use for us "
	typeName = 'const'
		ifTrue: [ self skipSpace.
			typeName := self parseWord.
			typeName ifNil: [ ^ self error: 'type name expected' ]
			].

	ptrArity := 0.
	[ stream atEnd not and: [ stream peek beginsWith: '*'] ] whileTrue: [ | pointerMarker |
		pointerMarker := stream next.
		self assert: (pointerMarker allSatisfy: [ :e | e = $* ]).
		ptrArity := ptrArity + pointerMarker size ].
	
	"Answer a tuple name, arity"
	^ {typeName. ptrArity}
]

{ #category : #accessing }
FFISplitFunctionParser >> setOn: aStreamSource [

	| functionDeclarationArray |
	functionDeclarationArray := aStreamSource isString ifTrue: [ | node |
			node := RBParser parseExpression: '#(', aStreamSource, ')'.
			node isLiteralNode ifFalse: [ self halt ].
			node evaluate
		] ifFalse: [ aStreamSource ].
			
	stream := functionDeclarationArray readStream.
]
