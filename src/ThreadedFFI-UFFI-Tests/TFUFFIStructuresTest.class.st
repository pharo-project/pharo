Class {
	#name : #TFUFFIStructuresTest,
	#superclass : #TFUFFITestCase,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> passStructByParameter: aPoint x: x y: y [

	^ self ffiCall: #(int assertCorrectPoint(TFPointTestStruct aPoint, int x, int y))
]

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> passStructByReference: aPoint [

	^ self ffiCall: #(void* id_pointer(TFPointTestStruct* aPoint))
]

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> passingLongStructByCopy: st b: b c: c d: d [

	^ self ffiCall: #(int passingLongStruct(TFLongTestStruct st, float b, double c, long d))
]

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> passingLongStructByReference: st b: b c: c d: d [

	^ self ffiCall: #(int passingLongStructByRef(TFLongTestStruct* st, float b, double c, long d))
]

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> passingNestedStructByCopy: st a: a y: y [
 
	^ self ffiCall: #(int passingNestedStruct(TFNestedTestStruct st, char a, double y))
]

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> returnNewPointX: x Y: y [

	^ self ffiCall: #(TFPointTestStruct newPoint(int x, int y))
]

{ #category : #'private - ffi' }
TFUFFIStructuresTest >> sizeOfNestedStruct [

	^ self ffiCall: #(int sizeOfNestedStruct())
]

{ #category : #tests }
TFUFFIStructuresTest >> testNestedStructSize [

	self assert: TFNestedTestStruct byteSize equals: self sizeOfNestedStruct
]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingByParameterAStructInC [

	| aPoint return |
	aPoint := TFPointTestStruct externalNew
					autoRelease;
					yourself.
	
	aPoint x: 21.
	aPoint y: 33.
	
	return := self passStructByParameter: aPoint x: 21 y: 33.
	self assert: return equals: 1.

]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingByParameterAStructInPharo [

	| aPoint return |
	aPoint := TFPointTestStruct new.
	
	aPoint x: 21.
	aPoint y: 33.
	
	return := self passStructByParameter: aPoint x: 21 y: 33.
	self assert: return equals: 1.

]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingByReferenceAStructInC [

	| aPoint return |
	aPoint := TFPointTestStruct externalNew
					autoRelease;
					yourself.
	
	aPoint x: 21.
	aPoint y: 33.
	
	return := self passStructByReference: aPoint.
	self assert: return asInteger equals: aPoint getHandle asInteger.

]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingLongStructByCopyInC [

	| aPoint return |
	aPoint := TFLongTestStruct externalNew
					autoRelease;
					yourself.
	
	aPoint b: 1.0.
	aPoint c: 2.0.
	aPoint d: 42.
	
	return := self passingLongStructByCopy: aPoint b: 1.0 c: 2.0 d: 42.
	self assert: return equals: 1.

]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingLongStructByCopyInPharo [

	| aPoint return |
	aPoint := TFLongTestStruct new.
	
	aPoint b: 1.0.
	aPoint c: 2.0.
	aPoint d: 42.
	
	return := self passingLongStructByCopy: aPoint b: 1.0 c: 2.0 d: 42.
	self assert: return equals: 1.

]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingLongStructByReferenceInC [

	| aPoint return |
	aPoint := TFLongTestStruct externalNew
					autoRelease;
					yourself.
	
	aPoint b: 1.0.
	aPoint c: 2.0.
	aPoint d: 42.
	
	return := self passingLongStructByReference: aPoint b: 1.0 c: 2.0 d: 42.
	self assert: return equals: 1.

]

{ #category : #tests }
TFUFFIStructuresTest >> testPassingNestedStructByCopyInC [

	| st return |
	st := TFNestedTestStruct externalNew
					autoRelease;
					yourself.
	
	st inner a: $a.
	st y: 1.0.
	
	return := self passingNestedStructByCopy: st a: $a y: 1.0.
	self assert: return equals: 1.

]

{ #category : #tests }
TFUFFIStructuresTest >> testReturningAnStruct [

	| aPoint |
	aPoint := self returnNewPointX: 1 Y: 2.

	self assert: aPoint x equals: 1.
	self assert: aPoint y equals: 2.	
]
