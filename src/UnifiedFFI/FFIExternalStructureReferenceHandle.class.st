"
I'm an offset accessor for nested structures. 
The idea is that I remap the accessing to an offseted byte array, thus providing access to the inner structure without copying the values. 
This is useful when you have nested structures, something like this: 

struct StructNested {
	int value;
}

struct StructA {
	int one;
	struct StructNested theNest;
}

then you can do something like this: 

s := StructA new.
s theNest value: 42. 

And the value will be set in the StructA instance, not in a copy as before. 
"
Class {
	#name : #FFIExternalStructureReferenceHandle,
	#superclass : #Object,
	#instVars : [
		'handle',
		'startOffset'
	],
	#category : #'UnifiedFFI-External-StructureHandling'
}

{ #category : #'instance creation' }
FFIExternalStructureReferenceHandle class >> handle: aHandle offset: aNumber [
	^ self basicNew 
		initializeHandle: aHandle offset: aNumber;
		yourself
]

{ #category : #'instance creation' }
FFIExternalStructureReferenceHandle class >> new [
	self error: 'Use #handle:offset:'
]

{ #category : #converting }
FFIExternalStructureReferenceHandle >> asExternalPointer [
	"Convert the receiver assuming that it describes a pointer to an object."
	^(ExternalAddress new)
		basicAt: 1 put: (self byteAt: startOffset + 1);
		basicAt: 2 put: (self byteAt: startOffset + 2);
		basicAt: 3 put: (self byteAt: startOffset + 3);
		basicAt: 4 put: (self byteAt: startOffset + 4);
	yourself
]

{ #category : #compatibility }
FFIExternalStructureReferenceHandle >> beNull [
	"this is referenced so this operation does not has sense. 
	 Included to provide polymophism"
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> booleanAt: byteOffset [
	^ handle booleanAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> booleanAt: byteOffset put: value [
	^ handle booleanAt: startOffset + byteOffset put: value
]

{ #category : #copying }
FFIExternalStructureReferenceHandle >> copyFrom: start to: stop [
	^ handle copyFrom: start + startOffset to: stop + startOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> doubleAt: byteOffset [
	^ handle doubleAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> doubleAt: byteOffset put: value [
	^ handle doubleAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> floatAt: byteOffset [
	^ handle floatAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> floatAt: byteOffset put: value [
	^ handle floatAt: startOffset + byteOffset put: value
]

{ #category : #compatibility }
FFIExternalStructureReferenceHandle >> free [
	"this is referenced so this operation does not has sense. 
	 Included to provide polymophism"
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> getHandle [
	^ handle
]

{ #category : #initialization }
FFIExternalStructureReferenceHandle >> initializeHandle: aHandle offset: aNumber [
	self initialize.
	handle := aHandle.
	startOffset := aNumber - 1 "starting offset is zero based"
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> integerAt: byteOffset put: value size: nBytes signed: aBoolean [
	^ handle integerAt: startOffset + byteOffset put: value size: nBytes signed: aBoolean
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> integerAt: byteOffset size: nBytes signed: aBoolean [
	^ handle integerAt: startOffset + byteOffset size: nBytes signed: aBoolean
]

{ #category : #testing }
FFIExternalStructureReferenceHandle >> isExternalAddress [
	^ handle isExternalAddress
]

{ #category : #testing }
FFIExternalStructureReferenceHandle >> isNull [
	self flag: #pharoTodo. "This is probably wrong"
	^ handle isNull
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> platformLongAt: byteOffset [
	^ handle platformLongAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> platformLongAt: byteOffset put: value [
	^ handle platformLongAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> platformSizeTAt: byteOffset [ 
	^ handle platformSizeTAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> platformSizeTAt: byteOffset put: aValue [ 
	^ handle platformSizeTAt: startOffset + byteOffset put: aValue
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> platformUnsignedLongAt: byteOffset [
	^ handle platformUnsignedLongAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> platformUnsignedLongAt: byteOffset put: value [
	^ handle platformUnsignedLongAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> pointerAt: byteOffset [
	^ handle pointerAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> pointerAt: byteOffset put: value [
	^ handle pointerAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> referenceStructAt: byteOffset length: length [
	^ handle referenceStructAt: startOffset + byteOffset length: length
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	^ handle replaceFrom: start + startOffset to: stop + startOffset with: replacement startingAt: repStart
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedByteAt: byteOffset [
	^ handle signedByteAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedByteAt: byteOffset put: value [
	^ handle signedByteAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedCharAt: byteOffset [
	^ handle signedCharAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedCharAt: byteOffset put: value [
	^ handle signedCharAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedLongAt: byteOffset [
	^ handle signedLongAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedLongAt: byteOffset put: value [
	^ handle signedLongAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedLongLongAt: byteOffset [
	^ handle signedLongLongAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedLongLongAt: byteOffset put: value [
	^ handle signedLongLongAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedShortAt: byteOffset [
	^ handle signedShortAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> signedShortAt: byteOffset put: value [
	^ handle signedShortAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> structAt: byteOffset length: length [
	^ handle structAt: startOffset + byteOffset length: length
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> structAt: byteOffset put: value length: length [
	"Store a structure of the given length starting at the indicated byte offset."
	1 to: length do:[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value unsignedByteAt: i)].
	^value	
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedByteAt: byteOffset [
	^ handle unsignedByteAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedByteAt: byteOffset put: value [
	^ handle unsignedByteAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedCharAt: byteOffset [
	^ handle unsignedCharAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedCharAt: byteOffset put: value [
	^ handle unsignedCharAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedLongAt: byteOffset [
	^ handle unsignedLongAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedLongAt: byteOffset put: value [
	^ handle unsignedLongAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedLongLongAt: byteOffset [
	^ handle unsignedLongLongAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedLongLongAt: byteOffset put: value [
	^ handle unsignedLongLongAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedShortAt: byteOffset [
	^ handle unsignedShortAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> unsignedShortAt: byteOffset put: value [
	^ handle unsignedShortAt: startOffset + byteOffset put: value
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> voidAt: byteOffset [
	^ handle voidAt: startOffset + byteOffset
]

{ #category : #accessing }
FFIExternalStructureReferenceHandle >> voidAt: byteOffset put: value [
	^ handle voidAt: startOffset + byteOffset put: value
]
