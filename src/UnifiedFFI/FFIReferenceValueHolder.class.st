"
This is a special `FFIValueHolder` for referenced objects (structs, opaque objects, etc.)
"
Class {
	#name : 'FFIReferenceValueHolder',
	#superclass : 'FFIValueHolder',
	#category : 'UnifiedFFI-Objects',
	#package : 'UnifiedFFI',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
FFIReferenceValueHolder >> arrayOfSize: aNumber [ 
	| first |

	first := self getHandle pointerAt: 1.
	^ Array streamContents: [ :stream |
		0 to: aNumber - 1 do: [ :index |
			| next value |
			next := first + (index * FFIExternalType pointerSize).
			value := self typeClass fromHandle: next.
			stream nextPut: value ] ]
]

{ #category : 'private' }
FFIReferenceValueHolder >> typeClass [

	^ self type class
]

{ #category : 'accessing' }
FFIReferenceValueHolder >> value: anOpaqueObject [

	^ super value: anOpaqueObject getHandle
]
