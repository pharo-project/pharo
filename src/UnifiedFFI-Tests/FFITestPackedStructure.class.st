"
A packaged structure for test purposes
"
Class {
	#name : #FFITestPackedStructure,
	#superclass : #FFIExternalPackedStructure,
	#classVars : [
		'OFFSET_BYTE',
		'OFFSET_DOUBLE',
		'OFFSET_FLOAT',
		'OFFSET_INT64',
		'OFFSET_LONG',
		'OFFSET_SHORT'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'field definition' }
FFITestPackedStructure class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^ #(
		byte byte;
		short short;
		long long;
		float float;
		double double;
		int64 int64;
		)
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> byte [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BYTE
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> byte: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BYTE put: anObject
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> double [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_DOUBLE
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> double: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_DOUBLE put: anObject
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> float [
	"This method was automatically generated"
	^handle floatAt: OFFSET_FLOAT
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> float: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_FLOAT put: anObject
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> int64 [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_INT64
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> int64: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_INT64 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> long [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LONG
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> long: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LONG put: anObject
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> short [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_SHORT
]

{ #category : #'accessing structure variables' }
FFITestPackedStructure >> short: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_SHORT put: anObject
]
