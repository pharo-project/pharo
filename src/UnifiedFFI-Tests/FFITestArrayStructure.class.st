"
An array  structure for test purposes
"
Class {
	#name : #FFITestArrayStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'Byte10',
		'Char5',
		'OFFSET_BYTE10',
		'OFFSET_CHAR5'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'field definition' }
FFITestArrayStructure class >> fieldsDesc [
" self rebuildFieldAccessors "
	^ #(
		Char5 char5;
		Byte10 byte10;
		)
]

{ #category : #'class initialization' }
FFITestArrayStructure class >> initialize [ 
	Char5 := FFITypeArray ofType: 'char' size: 5.
	Byte10 := FFITypeArray ofType: 'byte' size: 10.
]

{ #category : #'accessing structure variables' }
FFITestArrayStructure >> byte10 [
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFIUInt8 size: 10) fromHandle: (handle copyFrom: OFFSET_BYTE10 to: OFFSET_BYTE10 + 9)
]

{ #category : #'accessing structure variables' }
FFITestArrayStructure >> byte10: anObject [
	"This method was automatically generated"
	handle replaceFrom: OFFSET_BYTE10 to: OFFSET_BYTE10 + 9 with: anObject getHandle startingAt: 1
]

{ #category : #'accessing structure variables' }
FFITestArrayStructure >> char5 [
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFICharacterType size: 5) fromHandle: (handle copyFrom: OFFSET_CHAR5 to: OFFSET_CHAR5 + 4)
]

{ #category : #'accessing structure variables' }
FFITestArrayStructure >> char5: anObject [
	"This method was automatically generated"
	handle replaceFrom: OFFSET_CHAR5 to: OFFSET_CHAR5 + 4 with: anObject getHandle startingAt: 1
]
