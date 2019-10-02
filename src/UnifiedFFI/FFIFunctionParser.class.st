"
I parse function specs.
"
Class {
	#name : #FFIFunctionParser,
	#superclass : #Object,
	#instVars : [
		'requestor',
		'stream',
		'fnSpec'
	],
	#category : #'UnifiedFFI-Base'
}

{ #category : #'requestor callbacks' }
FFIFunctionParser >> argName: argName indirectIndex: anIndex type: aTypeName ptrArity: arity [

	argName ifNil: [ 
		"allow nil,true,false as untyped arguments"
		"Do not accept nil, true or false followed by *s"
		arity > 0 ifTrue: [ self error: 'missing argument name' ].
		^ FFIConstantArgument new
			value: aTypeName;
			yourself ].

	^ argName isSymbol ifTrue: [ 
		FFIVariableArgument
			name: argName
			typeName: aTypeName
			arity: arity ] ifFalse: [ 
		FFIConstantArgument
			value: argName
			typeName: aTypeName
			arity: arity ]
]

{ #category : #initialization }
FFIFunctionParser >> initialize [
	super initialize.
	requestor := self
]

{ #category : #'requestor callbacks' }
FFIFunctionParser >> integerConstantArgument: aValue [
	^ FFIConstantArgument new
		value: aValue;
		yourself
]

{ #category : #'requestor callbacks' }
FFIFunctionParser >> integerConstantArgument: anInteger type: aTypeName [

	^ FFIConstantArgument new
		value: anInteger;
		type: (FFITypeDeclaration typeName: aTypeName arity: 0);
		yourself
]

{ #category : #accessing }
FFIFunctionParser >> parseAnonymousFunction: aFunctionDefinition [
	"Parse a function definition in a form of: 
	
	 #( returnType (arguments) )
	
	 or

	 'returnType (arguments)'

	 Answer an instance of FFIFunctionSpec class"
	| argsSpec typeAndName |
	
	fnSpec := FFIFunctionSpec new.
	
	self setOn: aFunctionDefinition.
	
	argsSpec := stream contents last.
	typeAndName := stream contents allButLast.
		
	"Parse return type"
	stream := typeAndName readStream.
	fnSpec returnType: self parseReturn.

	stream atEnd ifFalse: [ self error: 'Nothing more expected after function type'].	

	"Function arguments"	
	stream := argsSpec readStream.
	self parseArguments.

	^ fnSpec
]

{ #category : #accessing }
FFIFunctionParser >> parseArgument [
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
		^ self integerConstantArgument: stream next ].
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
	argument := self
		argName: argName
		indirectIndex: indirectIndex
		type: typeAndPtrArity first
		ptrArity: typeAndPtrArity second.

	^ argument 
]

{ #category : #accessing }
FFIFunctionParser >> parseArgumentCastedType [
	| castedType number |

	castedType := self parseCastedType.
	number := stream next.
	^ self integerConstantArgument: number type: castedType
]

{ #category : #accessing }
FFIFunctionParser >> parseArguments [
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
FFIFunctionParser >> parseCastedType [

	^ (String streamContents: [ :castedType |
		stream next do: [ :typepart |
			castedType
				nextPutAll: typepart;
				space ] ]) trimBoth
]

{ #category : #parsing }
FFIFunctionParser >> parseInt [

	| negate num |

	negate := false.
	(stream peek = $-) ifTrue: [ 
		negate := true. 
		stream next.
		self skipSpace ].

	num := String new writeStream.	
	[ stream atEnd not and: [stream peek isDigit ] ] 
	whileTrue: [ num nextPut: stream next ].

	num := num contents asInteger.
	negate ifTrue: [ num := num negated ].
	
	^ num
]

{ #category : #accessing }
FFIFunctionParser >> parseNamedFunction: aFunctionDefinition [
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

{ #category : #parsing }
FFIFunctionParser >> parseReturn [

	^ self returnType: self parseType.
]

{ #category : #accessing }
FFIFunctionParser >> parseType [

	" parse type name and optional number of asterisks, following it"

	| typeName ptrArity |

	typeName := stream next.
	typeName ifNil: [ ^ self error: 'type name expected' ].	

	"skip 'const' , which is often used but has no any use for us "
	typeName = 'const'
		ifTrue: [
			typeName := stream next.
			typeName ifNil: [ ^ self error: 'type name expected' ]
			].

	ptrArity := 0.
	[ stream atEnd not and: [ stream peek isString and: [ stream peek beginsWith: '*' ] ] ] whileTrue: [ | pointerMarker |
		pointerMarker := stream next.
		self assert: (pointerMarker allSatisfy: [ :e | e = $* ]).
		ptrArity := ptrArity + pointerMarker size ].
	
	"Answer a tuple name, arity"
	^ {typeName. ptrArity}
]

{ #category : #parsing }
FFIFunctionParser >> parseWord [
	^ String streamContents: [:st | | ch |
		"first char must be letter or underscore"
		ch := stream peek.	
		(ch notNil and: [ ch isLetter or: [ '_$' includes: ch ]]) ifFalse: [ ^ nil ].
		[ 
			ch := stream peek. 
			ch notNil 
				and: [ 
					ch isLetter 
					or: [ ('_$' includes: ch)  
					or: [ch isDigit] ] ] ] 
		whileTrue: [ st nextPut: stream next ] ]


]

{ #category : #accessing }
FFIFunctionParser >> requestor [
	^ requestor
]

{ #category : #accessing }
FFIFunctionParser >> requestor: aRequestor [
	requestor := aRequestor
]

{ #category : #'requestor callbacks' }
FFIFunctionParser >> returnType: aType [
	^ FFITypeDeclaration typeName: aType first arity: aType second
]

{ #category : #accessing }
FFIFunctionParser >> setOn: aStreamSource [

	| functionDeclarationArray |
	functionDeclarationArray := aStreamSource isString ifTrue: [ | node |
			node := RBParser parseExpression: '#(', aStreamSource, ')'.
			node isLiteralNode ifFalse: [ self halt ].
			node evaluate
		] ifFalse: [ aStreamSource ].
			
	stream := functionDeclarationArray readStream.
]

{ #category : #accessing }
FFIFunctionParser >> setStream: aReadStream [
	
	stream := aReadStream
]

{ #category : #parsing }
FFIFunctionParser >> skipSpace [
	| ch |

	[ 
		ch := stream peek.
		ch ifNil: [ ^ self ].
		ch isSeparator ] 
	whileTrue: [ stream next ]
]
