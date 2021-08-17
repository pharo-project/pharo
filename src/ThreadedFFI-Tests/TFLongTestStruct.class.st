"
I am a test structure fill with longs
"
Class {
	#name : #TFLongTestStruct,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'Char100',
		'OFFSET_A',
		'OFFSET_B',
		'OFFSET_C',
		'OFFSET_D',
		'OFFSET_E',
		'OFFSET_F'
	],
	#category : #'ThreadedFFI-Tests-Test-Structs'
}

{ #category : #'field definition' }
TFLongTestStruct class >> fieldsDesc [
	
	"self rebuildFieldAccessors"
	
	^ #(
	Char100 a;
	float b;
	double c;
	long d;
	int e;
	short f;
	)
]

{ #category : #'field definition' }
TFLongTestStruct class >> initialize [
	Char100 := FFITypeArray ofType: 'char' size: 100
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> a [
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFICharacterType size: 100) fromHandle: (handle copyFrom: OFFSET_A to: OFFSET_A + 99)
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> a: anObject [
	"This method was automatically generated"
	handle replaceFrom: OFFSET_A to: OFFSET_A + 99 with: anObject getHandle startingAt: 1
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> b [
	"This method was automatically generated"
	^handle floatAt: OFFSET_B
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> b: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_B put: anObject
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> c [
	"This method was automatically generated"
	^handle doubleAt: OFFSET_C
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> c: anObject [
	"This method was automatically generated"
	handle doubleAt: OFFSET_C put: anObject
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> d [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_D
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> d: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_D put: anObject
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> e [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_E
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> e: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_E put: anObject
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> f [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_F
]

{ #category : #'accessing - structure variables' }
TFLongTestStruct >> f: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_F put: anObject
]
