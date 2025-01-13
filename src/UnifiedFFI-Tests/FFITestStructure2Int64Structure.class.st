"
A structure containing 2 int64 values as used by UFFI tests
"
Class {
	#name : #FFITestStructure2Int64Structure,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : #'UnifiedFFI-Tests-Test-Data'
}

{ #category : #'field definition' }
FFITestStructure2Int64Structure class >> fieldsDesc [
	"self rebuildFieldAccessors"
	^ #(
	int64 x;
	int64 y;
	)
]

{ #category : #'accessing - structure variables' }
FFITestStructure2Int64Structure >> x [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
FFITestStructure2Int64Structure >> x: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
FFITestStructure2Int64Structure >> y [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
FFITestStructure2Int64Structure >> y: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_Y put: anObject
]
