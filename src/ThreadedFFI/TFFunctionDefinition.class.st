"
I represent a function type signature. I am used for both defining named functions and anonymous callbacks.
I am created using an array of parameter types and a single return type, all TFBasicType (sub)instances.

TFFunctionDefinition
	parameterTypes: {TFBasicType sint. TFBasicType float.}
	returnType: TFBasicType float.
	
I am not meant to be used from outside of this plugin, so my main users are TFCallback and TFExternalFunction
"
Class {
	#name : #TFFunctionDefinition,
	#superclass : #FFIExternalReference,
	#instVars : [
		'parameterTypes',
		'returnType'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'instance creation' }
TFFunctionDefinition class >> finalizeResourceData: handle [

	handle isNull ifTrue: [ ^ self ].

	self new
		setHandle: handle;
		primFreeDefinition.
]

{ #category : #'instance creation' }
TFFunctionDefinition class >> fromHandle: aHandle [
	
	^ self error: 'should not be used'
]

{ #category : #'instance creation' }
TFFunctionDefinition class >> parameterTypes: someParameters returnType: returnType [

	^ self new
		parameterTypes: someParameters;
		returnType: returnType;
		autoRelease;
		yourself
]

{ #category : #private }
TFFunctionDefinition >> doValidate: parameterHandlers [

	self primDefineFunctionWith: parameterHandlers returnType: returnType getHandle
]

{ #category : #accessing }
TFFunctionDefinition >> parameterTypes [
	^ parameterTypes
]

{ #category : #accessing }
TFFunctionDefinition >> parameterTypes: anObject [
	parameterTypes := anObject
]

{ #category : #primitives }
TFFunctionDefinition >> primDefineFunctionWith: parameterHandlers returnType: returnTypeHandler [ 
	
	<primitive: 'primitiveDefineFunction' error: ec>

	^ self primitiveFailed	
]

{ #category : #primitives }
TFFunctionDefinition >> primFreeDefinition [
	
	<primitive: 'primitiveFreeDefinition'>

	^ self primitiveFailed	
]

{ #category : #accessing }
TFFunctionDefinition >> returnType [
	^ returnType
]

{ #category : #accessing }
TFFunctionDefinition >> returnType: anObject [
	returnType := anObject
]

{ #category : #accessing }
TFFunctionDefinition >> validate [
	| parameterHandlers |

	handle isNull ifFalse: [ ^self ].

	"First, we get valid parameter handles."
	parameterTypes do: [ :e | e validate ].
	returnType validate.
	
	parameterHandlers := parameterTypes collect: [ :e | e getHandle ] as: Array.
	
	self doValidate: parameterHandlers.

]
