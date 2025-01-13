"
A structure containing double values as used by UFFI tests
"
Class {
	#name : #FFITestStructureDoubleStructure,
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
FFITestStructureDoubleStructure class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	double x;
	double y;
	double z;
	double w;
	)
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> w [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_W
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> w: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_W put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> x [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> x: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> y [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> y: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Y put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> z [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Z
]

{ #category : #'accessing - structure variables' }
FFITestStructureDoubleStructure >> z: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Z put: anObject
]
