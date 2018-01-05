"
A structure for test purposes
"
Class {
	#name : #FFITestStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_BYTE',
		'OFFSET_DOUBLE',
		'OFFSET_FLOAT',
		'OFFSET_INT64',
		'OFFSET_LONG',
		'OFFSET_SHORT',
		'OFFSET_ULONG'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'field definition' }
FFITestStructure class >> fieldsDesc [
" self rebuildFieldAccessors "
	^ #(
		byte byte;
		short short;
		long long;
		float float;
		double double;
		int64 int64;
		ulong ulong;
		)
]

{ #category : #'class initialization' }
FFITestStructure class >> initialize [
	"self rebuildFieldAccessors"
]

{ #category : #'accessing structure variables' }
FFITestStructure >> byte [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BYTE
]

{ #category : #'accessing structure variables' }
FFITestStructure >> byte: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BYTE put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructure >> double [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_DOUBLE
]

{ #category : #'accessing structure variables' }
FFITestStructure >> double: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_DOUBLE put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructure >> float [
	"This method was automatically generated"
	^handle floatAt: OFFSET_FLOAT
]

{ #category : #'accessing structure variables' }
FFITestStructure >> float: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_FLOAT put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructure >> int64 [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_INT64
]

{ #category : #'accessing structure variables' }
FFITestStructure >> int64: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_INT64 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructure >> long [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LONG
]

{ #category : #'accessing structure variables' }
FFITestStructure >> long: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LONG put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructure >> short [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_SHORT
]

{ #category : #'accessing structure variables' }
FFITestStructure >> short: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_SHORT put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructure >> ulong [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_ULONG
]

{ #category : #'accessing structure variables' }
FFITestStructure >> ulong: anObject [
	"This method was automatically generated"
	^handle platformUnsignedLongAt: OFFSET_ULONG put: anObject
]
