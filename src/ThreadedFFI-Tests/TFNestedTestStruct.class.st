"
I am an struct with another inside.
"
Class {
	#name : #TFNestedTestStruct,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_INNER',
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : #'ThreadedFFI-Tests-Test-Structs'
}

{ #category : #'field definition' }
TFNestedTestStruct class >> fieldsDesc [
	
	"self rebuildFieldAccessors"
		
	^ #(
	TFInnerTestStruct inner;
	float x;
	double y;
	)
]

{ #category : #'accessing - structure variables' }
TFNestedTestStruct >> inner [
	"This method was automatically generated"
	^ TFInnerTestStruct fromHandle: (handle referenceStructAt: OFFSET_INNER length: TFInnerTestStruct byteSize)
]

{ #category : #'accessing - structure variables' }
TFNestedTestStruct >> inner: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_INNER put: anObject getHandle length: TFInnerTestStruct byteSize
]

{ #category : #'accessing - structure variables' }
TFNestedTestStruct >> x [
	"This method was automatically generated"
	^handle floatAt: OFFSET_X
]

{ #category : #'accessing - structure variables' }
TFNestedTestStruct >> x: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_X put: anObject
]

{ #category : #'accessing - structure variables' }
TFNestedTestStruct >> y [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_Y
]

{ #category : #'accessing - structure variables' }
TFNestedTestStruct >> y: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_Y put: anObject
]
