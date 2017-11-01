"
I'm an external value holder type (see ==FFIExternalValueHolder==  comment).
"
Class {
	#name : #FFIExternalValueHolderType,
	#superclass : #FFIExternalReferenceType,
	#instVars : [
		'representationType'
	],
	#category : 'UnifiedFFI-Types'
}

{ #category : #'instance creation' }
FFIExternalValueHolderType class >> objectClass: aClass [
	^ self objectClass: aClass representationType: FFIUInt32 new
]

{ #category : #'instance creation' }
FFIExternalValueHolderType class >> objectClass: aClass representationType: anIntegerType [
	^ self new
		objectClass: aClass;
		representationType: anIntegerType
]

{ #category : #accessing }
FFIExternalValueHolderType >> externalType [
	^ self representationType externalType asPointerType
]

{ #category : #accessing }
FFIExternalValueHolderType >> externalTypeSize [ 
	^ self pointerSize
]

{ #category : #accessing }
FFIExternalValueHolderType >> instanceVariableName [
	^ #data
]

{ #category : #'emitting code' }
FFIExternalValueHolderType >> offsetReadFieldAt: offsetVariableName [
	^ self notYetImplemented
]

{ #category : #'emitting code' }
FFIExternalValueHolderType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	^ self notYetImplemented
]

{ #category : #'emitting code' }
FFIExternalValueHolderType >> readFieldAt: byteOffset [	
	^ self notYetImplemented
]

{ #category : #accessing }
FFIExternalValueHolderType >> representationType [
	^ representationType
]

{ #category : #accessing }
FFIExternalValueHolderType >> representationType: anObject [
	representationType := anObject
]

{ #category : #'emitting code' }
FFIExternalValueHolderType >> writeFieldAt: byteOffset with: valueName [
	^ self notYetImplemented
]
