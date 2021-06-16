"
I am a point like structure used for test
"
Class {
	#name : #TFPointTestStruct,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : #'ThreadedFFI-Tests-Test-Structs'
}

{ #category : #'field definition' }
TFPointTestStruct class >> fieldsDesc [
	
	"self rebuildFieldAccessors"
	
	^ #(
		int x
		int y
	)
]

{ #category : #'accessing - structure variables' }
TFPointTestStruct >> x [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
TFPointTestStruct >> x: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
TFPointTestStruct >> y [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
TFPointTestStruct >> y: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_Y put: anObject
]
