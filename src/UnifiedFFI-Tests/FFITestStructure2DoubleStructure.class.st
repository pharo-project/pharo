"
A structure containing 2 double values as used by UFFI tests
"
Class {
	#name : #FFITestStructure2DoubleStructure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : #'UnifiedFFI-Tests-Test-Data'
}

{ #category : #'field definition' }
FFITestStructure2DoubleStructure class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	double x;
	double y;
	)
]

{ #category : #'accessing - structure variables' }
FFITestStructure2DoubleStructure >> x [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
FFITestStructure2DoubleStructure >> x: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructure2DoubleStructure >> y [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
FFITestStructure2DoubleStructure >> y: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Y put: anObject
]
