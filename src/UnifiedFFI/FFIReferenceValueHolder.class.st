"
This is a special `FFIValueHolder` for referenced objects (structs, opaque objects, external objects)
"
Class {
	#name : 'FFIReferenceValueHolder',
	#superclass : 'FFIValueHolder',
	#instVars : [
		'typeSize'
	],
	#category : 'UnifiedFFI-Objects',
	#package : 'UnifiedFFI',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
FFIReferenceValueHolder class >> newType: aType [

	^ self 
		newType: aType 
		handle: FFIValueHolderHandle new
]

{ #category : 'instance creation' }
FFIReferenceValueHolder class >> newType: aType size: aNumber [

	^ (self newType: aType)
		typeSize: aNumber;
		yourself
]

{ #category : 'accessing' }
FFIReferenceValueHolder >> arrayOfSize: aNumber [
			
	^ self
		arrayOf: self typeClass 
		size: aNumber
]

{ #category : 'private' }
FFIReferenceValueHolder >> basicArrayOfType: aType size: aNumber [
	| next |

	aType isExternalStructure 
		ifFalse: [ ^ super basicArrayOfType: aType size: aNumber ].
	
	next := self firstHandle.
	^ Array streamContents: [ :stream |
		aNumber timesRepeat: [
			| value |
			value := self typeClass fromHandle: next.
			stream nextPut: value.
			next := next + self typeSize ] ]
]

{ #category : 'private' }
FFIReferenceValueHolder >> firstHandle [

	^ ExternalAddress new 
		adoptAddress: self getHandle;
		yourself
]

{ #category : 'private' }
FFIReferenceValueHolder >> typeClass [

	^ self type class
]

{ #category : 'private' }
FFIReferenceValueHolder >> typeSize [

	^ typeSize
]

{ #category : 'initialization' }
FFIReferenceValueHolder >> typeSize: aNumber [ 

	typeSize := aNumber
]

{ #category : 'accessing' }
FFIReferenceValueHolder >> value: anOpaqueObject [

	^ super value: anOpaqueObject getHandle
]
