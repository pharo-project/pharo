"
I am an struct used for testing.
I am the inner struct of a bigger one.
"
Class {
	#name : 'TFInnerTestStruct',
	#superclass : 'FFIExternalStructure',
	#classVars : [
		'OFFSET_A',
		'OFFSET_B'
	],
	#category : 'ThreadedFFI-Tests-Test-Structs',
	#package : 'ThreadedFFI-Tests',
	#tag : 'Test-Structs'
}

{ #category : 'field definition' }
TFInnerTestStruct class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
	char a;
	void* b;
	)
]

{ #category : 'accessing - structure variables' }
TFInnerTestStruct >> a [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_A
]

{ #category : 'accessing - structure variables' }
TFInnerTestStruct >> a: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_A put: anObject
]

{ #category : 'accessing - structure variables' }
TFInnerTestStruct >> b [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_B) type: ExternalType void asPointerType
]

{ #category : 'accessing - structure variables' }
TFInnerTestStruct >> b: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_B put: anObject getHandle.
]
