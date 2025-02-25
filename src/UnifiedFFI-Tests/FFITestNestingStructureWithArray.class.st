"
A nesting structure with an array for test purposes
"
Class {
	#name : 'FFITestNestingStructureWithArray',
	#superclass : 'FFIExternalStructure',
	#classVars : [
		'OFFSET_NESTED'
	],
	#category : 'UnifiedFFI-Tests-Test-Data',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Test-Data'
}

{ #category : 'field definition' }
FFITestNestingStructureWithArray class >> fieldsDesc [
" self rebuildFieldAccessors "
	^ #(
			FFITestArrayStructure nested;
		)
]

{ #category : 'accessing - structure variables' }
FFITestNestingStructureWithArray >> nested [
	"This method was automatically generated"
	^ FFITestArrayStructure fromHandle: (handle referenceStructAt: OFFSET_NESTED length: FFITestArrayStructure byteSize)
]

{ #category : 'accessing - structure variables' }
FFITestNestingStructureWithArray >> nested: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_NESTED put: anObject getHandle length: FFITestArrayStructure byteSize
]
