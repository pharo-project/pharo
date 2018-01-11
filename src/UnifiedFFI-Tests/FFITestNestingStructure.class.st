"
A nesting structure for test purposes
"
Class {
	#name : #FFITestNestingStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_NESTED',
		'OFFSET_ONE',
		'OFFSET_TWO'
	],
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'field definition' }
FFITestNestingStructure class >> fieldsDesc [
" self rebuildFieldAccessors "
	^ #(
		byte one;
		long two;
		FFITestStructure nested;
		)
]

{ #category : #'class initialization' }
FFITestNestingStructure class >> initialize [
	"self rebuildFieldAccessors"
]

{ #category : #'accessing structure variables' }
FFITestNestingStructure >> nested [
	"This method was automatically generated"
	^FFITestStructure fromHandle: (handle referenceStructAt: OFFSET_NESTED length: 28)
]

{ #category : #'accessing structure variables' }
FFITestNestingStructure >> nested: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_NESTED put: anObject getHandle length: 28.
]

{ #category : #'accessing structure variables' }
FFITestNestingStructure >> one [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_ONE
]

{ #category : #'accessing structure variables' }
FFITestNestingStructure >> one: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_ONE put: anObject
]

{ #category : #'accessing structure variables' }
FFITestNestingStructure >> two [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_TWO
]

{ #category : #'accessing structure variables' }
FFITestNestingStructure >> two: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_TWO put: anObject
]
