"
I represent a function type signature. I am used for both defining named functions and anonymous callbacks.
I am created using an array of parameter types and a single return type, all TFBasicType (sub)instances.

TFFunctionDefinition
	parameterTypes: {TFBasicType sint. TFBasicType float.}
	returnType: TFBasicType float.
	
I am not meant to be used from outside of this plugin, so my main users are TFCallback and TFExternalFunction

As we maybe keep the function definition when we are saving the image, we cannot put a default value to the ABI, as the default value changes from platform to platform.
That is why the ABI is initialized to nil and then different primitives are used.

"
Class {
	#name : #TFFunctionDefinition,
	#superclass : #FFIExternalReference,
	#instVars : [
		'parameterTypes',
		'returnType',
		'abi'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #'instance creation' }
TFFunctionDefinition class >> finalizeResourceData: handle [

	handle isNull ifTrue: [ ^ self ].

	self new
		setHandle: handle;
		primFreeDefinition
]

{ #category : #'instance creation' }
TFFunctionDefinition class >> fromHandle: aHandle [

	^ self error: 'should not be used'
]

{ #category : #'instance creation' }
TFFunctionDefinition class >> parameterTypes: someParameters returnType: returnType [

	^ self parameterTypes: someParameters returnType: returnType abi: nil
]

{ #category : #'instance creation' }
TFFunctionDefinition class >> parameterTypes: someParameters returnType: returnType abi: abi [

	^ self new
		parameterTypes: someParameters;
		returnType: returnType;
		abi: abi;
		autoRelease;
		yourself
]

{ #category : #accessing }
TFFunctionDefinition >> abi [

	^ abi
]

{ #category : #accessing }
TFFunctionDefinition >> abi: anObject [

	abi := anObject
]

{ #category : #private }
TFFunctionDefinition >> doValidate: parameterHandlers [

	"As we maybe keep the function definition when we are saving the image, we cannot put a default value to the ABI, as the default value changes from platform to platform"

	abi
		ifNil: [
			self
				primDefineFunctionWith: parameterHandlers
				returnType: returnType getHandle ]
		ifNotNil: [
			self
				primDefineFunctionWith: parameterHandlers
				returnType: returnType getHandle
				abi: abi asInteger ]
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
TFFunctionDefinition >> primDefineFunctionWith: parameterHandlers returnType: returnTypeHandler abi: abiType [

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

	self doValidate: parameterHandlers
]
