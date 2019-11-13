Class {
	#name : #FFIAbstractTest,
	#superclass : #TestCase,
	#instVars : [
		'intType',
		'int32Type',
		'voidType',
		'charType',
		'uint32Type'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #testing }
FFIAbstractTest class >> isAbstract [

	^ self == FFIAbstractTest
]

{ #category : #private }
FFIAbstractTest >> externalTypeAlias: aTypeName [

	"Prefix the type so we have control over it.
	If it is already prefixed, do not prefix it (otherwise this loops)."
	(aTypeName beginsWith: '_test_type_')
		ifTrue: [ ^ aTypeName ].
	^ '_test_type_', aTypeName
]

{ #category : #private }
FFIAbstractTest >> ffiBindingOf: aString [
	
	aString = '_test_type_int'
		ifTrue: [ ^ aString -> intType ].
	aString = '_test_type_bool'
		ifTrue: [ ^ aString -> int32Type ].
	aString = '_test_type_uint32'
		ifTrue: [ ^ aString -> int32Type ].
	aString = '_test_type_int32'
		ifTrue: [ ^ aString -> int32Type ].
	aString = '_test_type_void'
		ifTrue: [ ^ aString -> voidType ].
	aString = '_test_type_char'
		ifTrue: [ ^ aString -> charType ].
	self error: 'Type not recognized: ', aString
]

{ #category : #running }
FFIAbstractTest >> setUp [

	super setUp.
	intType := FFIInt32.
	int32Type := FFIInt32.
	uint32Type := FFIUInt32.
	voidType := FFIVoid.
	charType := FFICharacterType.
]
