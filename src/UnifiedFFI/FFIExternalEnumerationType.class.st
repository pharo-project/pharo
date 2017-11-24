"
I define the type for FFIExternalEnumeration objects.
"
Class {
	#name : #FFIExternalEnumerationType,
	#superclass : #FFIExternalReferenceType,
	#instVars : [
		'representationType'
	],
	#category : #'UnifiedFFI-Types'
}

{ #category : #private }
FFIExternalEnumerationType class >> naturalPointerArity [
	^ 0
]

{ #category : #'instance creation' }
FFIExternalEnumerationType class >> objectClass: aClass [
	^ self objectClass: aClass representationType: FFIUInt32 new
]

{ #category : #'instance creation' }
FFIExternalEnumerationType class >> objectClass: aClass representationType: anIntegerType [
	^ self new
		objectClass: aClass;
		representationType: anIntegerType
]

{ #category : #private }
FFIExternalEnumerationType >> basicHandle: aHandle at: index [
	^ self objectClass fromInteger: (aHandle 
		perform: self externalType atomicSelector 
		with: index)
]

{ #category : #private }
FFIExternalEnumerationType >> basicHandle: aHandle at: index put: value [
	^ aHandle 
		perform: (self externalType atomicSelector,#put:) asSymbol
		with: index
		with: value getHandle
]

{ #category : #accessing }
FFIExternalEnumerationType >> callbackReturnOn: callbackContext for: anObject [
    ^ super callbackReturnOn: callbackContext for: anObject value
]

{ #category : #accessing }
FFIExternalEnumerationType >> externalType [
	^ self representationType externalType
]

{ #category : #accessing }
FFIExternalEnumerationType >> externalTypeAlignment [
	^ self representationType externalTypeAlignment
]

{ #category : #accessing }
FFIExternalEnumerationType >> externalTypeSize [ 
	^ self representationType externalTypeSize
]

{ #category : #accessing }
FFIExternalEnumerationType >> instanceVariableName [
	^ #value
]

{ #category : #'emitting code' }
FFIExternalEnumerationType >> offsetReadEnumAt: offsetVariableName [
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromInteger: (handle ', self externalType atomicSelector,' ';
			nextPutAll: offsetVariableName;
			nextPutAll:')' ].

]

{ #category : #'emitting code' }
FFIExternalEnumerationType >> offsetReadFieldAt: offsetVariableName [
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super offsetReadFieldAt: offsetVariableName ].
	^ self offsetReadEnumAt: offsetVariableName
]

{ #category : #'emitting code' }
FFIExternalEnumerationType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	^ String streamContents: [ :stream | 
		stream << 'handle ' << self externalType atomicSelector << ' ' << offsetVariableName << ' put: anObject value' ]
]

{ #category : #'emitting code' }
FFIExternalEnumerationType >> readEnumAt: byteOffset [
	^ String streamContents:[ :stream|
		stream 
			nextPutAll:'^';
			print: self objectClass;
			nextPutAll:' fromInteger: (handle ', self externalType atomicSelector,' ';
			print: byteOffset;
			nextPutAll:')' ].

]

{ #category : #'emitting code' }
FFIExternalEnumerationType >> readFieldAt: byteOffset [
	"Return a string defining the accessor to an entity of the receiver type starting at the given byte offset. "
	self isPointer ifTrue: [ ^ super readFieldAt: byteOffset ].
	^ self readEnumAt: byteOffset
]

{ #category : #accessing }
FFIExternalEnumerationType >> representationType [
	^ representationType
]

{ #category : #accessing }
FFIExternalEnumerationType >> representationType: anObject [
	representationType := anObject
]

{ #category : #'emitting code' }
FFIExternalEnumerationType >> writeFieldAt: byteOffset with: valueName [
	^ String streamContents: [ :stream | 
		stream << 'handle ' << self externalType atomicSelector << ' ' << byteOffset asString << ' put: anObject value' ]
]
