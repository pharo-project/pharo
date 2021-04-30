"
I represent an external function from a library.
I have a name, a TFFunctionDefinition type signature and I am fetched from an external library.

TFExternalFunction
	name: 'functionName'
	moduleName: 'pathToLibrary'
	definition: (TFFunctionDefinition
		parameterTypes: { TFBasicType pointer }
		returnType: TFBasicType sint).

To execute myself you can call my #execute and #executeWithArguments: method.
These will create an async function call and block the current pharo process while waiting for its finish.

!! Examples

((TFExternalFunction
	name: 'getenv'
	moduleName: LibC uniqueInstance moduleName
	parameterTypes: { TFBasicType pointer }
	returnType: TFBasicType pointer)
		executeWithArguments: { ExternalAddress fromString: 'HOME' utf8Encoded }) readString asByteArray utf8Decoded.
"
Class {
	#name : #TFExternalFunction,
	#superclass : #Object,
	#instVars : [
		'handle',
		'definition',
		'functionName',
		'moduleName'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'instance creation' }
TFExternalFunction class >> fromAddress: anExternalAddress definition: aFunctionDefinition [

	^ self new
		address: anExternalAddress;
		definition: aFunctionDefinition;
		yourself
]

{ #category : #'instance creation' }
TFExternalFunction class >> name: aName moduleName: aModuleName definition: aFunctionDefinition [

	^ self new
		address: ExternalAddress new;
		moduleName: aModuleName;
		functionName: aName;
		definition: aFunctionDefinition;
		yourself
]

{ #category : #'instance creation' }
TFExternalFunction class >> name: aName moduleName: aModuleName parameterTypes: parameterTypes returnType: returnType [

	^ self name: aName moduleName: aModuleName parameterTypes: parameterTypes returnType: returnType fixedArgumentCount: 0
]

{ #category : #'instance creation' }
TFExternalFunction class >> name: aName moduleName: aModuleName parameterTypes: parameterTypes returnType: returnType fixedArgumentCount: fixedArgumentCount [

	| definition |

	definition := fixedArgumentCount = 0 
		ifTrue:[	
			TFFunctionDefinition 
				parameterTypes: parameterTypes
				returnType: returnType]
		ifFalse:[	
			TFVariadicFunctionDefinition 
				parameterTypes: parameterTypes
				returnType: returnType
				fixedArgumentCount: fixedArgumentCount].

	^ self
		name: aName
		moduleName: aModuleName
		definition: definition
]

{ #category : #'instance creation' }
TFExternalFunction class >> name: aName moduleName: aModuleName parameterTypes: parameterTypes returnType: returnType type: aFunctionCallType [

	^ (self name: aName moduleName: aModuleName parameterTypes: parameterTypes returnType: returnType)
		type: aFunctionCallType;
		yourself

]

{ #category : #accessing }
TFExternalFunction >> address [
	^ handle
]

{ #category : #accessing }
TFExternalFunction >> address: anObject [
	handle := anObject
]

{ #category : #accessing }
TFExternalFunction >> definition [
	^ definition
]

{ #category : #accessing }
TFExternalFunction >> definition: anObject [
	definition := anObject
]

{ #category : #accessing }
TFExternalFunction >> functionName [
	^ functionName
]

{ #category : #accessing }
TFExternalFunction >> functionName: anObject [
	functionName := anObject
]

{ #category : #accessing }
TFExternalFunction >> moduleName [
	^ moduleName
]

{ #category : #accessing }
TFExternalFunction >> moduleName: anObject [
	moduleName := anObject
]

{ #category : #operations }
TFExternalFunction >> validate [
	definition validate.
	handle isNull
		ifTrue: [ handle := ExternalAddress loadSymbol: functionName module: moduleName ]
]
