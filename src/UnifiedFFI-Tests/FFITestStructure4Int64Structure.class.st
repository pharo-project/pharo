"
A structure containing 4 int64 values as used by UFFI tests
"
Class {
	#name : #FFITestStructure4Int64Structure,
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
FFITestStructure4Int64Structure class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	int64 x;
	int64 y;
	int64 z;
	int64 w;
	)
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> w [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_W
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> w: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_W put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> x [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> x: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> y [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> y: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_Y put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> z [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_Z
]

{ #category : #'accessing - structure variables' }
FFITestStructure4Int64Structure >> z: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_Z put: anObject
]
