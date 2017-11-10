"
I am a structure that in the AMD64 SystemV ABI is passed in 2 float registers.
"
Class {
	#name : #FFITestStructureSmallFloatStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_W',
		'OFFSET_X',
		'OFFSET_Y',
		'OFFSET_Z'
	],
	#category : #UnifiedFFI-Tests
}

{ #category : #'field definition' }
FFITestStructureSmallFloatStructure class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	float x;
	float y;
	float z;
	float w;
	)
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> w [
	"This method was automatically generated"
	^handle floatAt: OFFSET_W
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> w: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_W put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> x [
	"This method was automatically generated"
	^handle floatAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> x: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> y [
	"This method was automatically generated"
	^handle floatAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> y: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_Y put: anObject
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> z [
	"This method was automatically generated"
	^handle floatAt: OFFSET_Z
]

{ #category : #'accessing structure variables' }
FFITestStructureSmallFloatStructure >> z: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_Z put: anObject
]
