"
A structure containing int values as used by UFFI tests
"
Class {
	#name : #FFITestStructureIntStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_W',
		'OFFSET_X',
		'OFFSET_Y',
		'OFFSET_Z'
	],
	#category : #'UnifiedFFI-Tests-Test-Data'
}

{ #category : #'field definition' }
FFITestStructureIntStructure class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	int32 x;
	int32 y;
	int32 z;
	int32 w;
	)
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> w [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_W
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> w: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_W put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> x [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> x: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> y [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> y: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_Y put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> z [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_Z
]

{ #category : #'accessing - structure variables' }
FFITestStructureIntStructure >> z: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_Z put: anObject
]
