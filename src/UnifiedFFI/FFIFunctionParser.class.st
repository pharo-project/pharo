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
	#category : #UnifiedFFI
}

{ #category : #'requestor callbacks' }
FFIFunctionParser >> argName: aName indirectIndex: anIndex type: aTypeName ptrArity: arity [
	^ { aName. anIndex. aTypeName. arity }
]

{ #category : #initialization }
FFIFunctionParser >> initialize [
	super initialize.
	requestor := self
]

{ #category : #'requestor callbacks' }
FFIFunctionParser >> integerConstantArgument: aValue [
	^ aValue
]

{ #category : #accessing }
FFIFunctionParser >> parseAnonymousFunction: aFunctionDefinition [
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
]

{ #category : #parsing }
FFIFunctionParser >> parseArgument [
	| argument argName indirectIndex typeAndPtrArity char |
	" An argument can be:

 - nil , true , false
 - a class/pool variable name
 - integer literal

 - self , with optional **
 - type name *** arg name

"
	char := stream peek.
	(char isDigit or: [ char = $- ])
		ifTrue: [ ^ self requestor integerConstantArgument: self parseInt ].
	typeAndPtrArity := self parseType.
	self skipSpace.
	argName := self parseWord.
	self skipSpace.
	char := stream peek.
	char = $@
		ifTrue: [ 
			stream next.
			self skipSpace.
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
	argument isExternalType
		ifTrue: [ argument validateAfterParse: typeAndPtrArity ].
	^ argument 
]

{ #category : #parsing }
FFIFunctionParser >> parseArguments [
"
Parse a arguments spec in a form of: 

#( type1 name1 , type2 name2 , nil, true, false , 100 )
"
	[ stream atEnd ] whileFalse: [
		self parseArgument ifNotNil: [:arg | 
			fnSpec addArgument: arg].
		
		self skipSpace.
		
		stream atEnd ifFalse: [ stream peek = $, ifFalse: [ self error: 'comma expected' ].
			stream next. self skipSpace. ]
	].
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
]

{ #category : #parsing }
FFIFunctionParser >> parseReturn [
	| returnTypeTuple returnType |
	returnTypeTuple := self parseType.
	returnType := self requestor returnType: returnTypeTuple.
	returnType isExternalType
		ifTrue: [ returnType validateAfterParse: returnTypeTuple ].
	^ returnType
]

{ #category : #parsing }
FFIFunctionParser >> parseType [
	" parse type name and optional number of asterisks, following it"
	| typeName ptrArity |

	typeName := self parseWord.
	typeName isNil ifTrue: [ ^ self error: 'type name expected' ].

	"skip 'const' , which is often used but has no any use for us "	
	typeName = 'const' ifTrue: [
		self skipSpace.
		typeName := self parseWord.
		typeName isNil ifTrue: [ ^ self error: 'type name expected' ] ].
	
	ptrArity := 0.
	[ self skipSpace. stream peek = $* ] 
	whileTrue: [ 
		ptrArity := ptrArity + 1.
		stream next ].

	"Answer a tuple name, arity"
	^ { typeName. ptrArity }
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
	^ aType
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
