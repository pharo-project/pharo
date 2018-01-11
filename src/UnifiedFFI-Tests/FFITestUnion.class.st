"
A union for test purposes
"
Class {
	#name : #FFITestUnion,
	#superclass : #FFIExternalUnion,
	#category : #'UnifiedFFI-Tests'
}

{ #category : #'as yet unclassified' }
FFITestUnion class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^ #(
		byte byte;
		short short;
		long long;
		float float;
		double double;
		int64 int64;
		)
]

{ #category : #'accessing structure variables' }
FFITestUnion >> byte [
	"This method was automatically generated"
	^handle unsignedByteAt: 1
]

{ #category : #'accessing structure variables' }
FFITestUnion >> byte: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: 1 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestUnion >> double [
	"This method was automatically generated"
	^handle doubleAt: 1
]

{ #category : #'accessing structure variables' }
FFITestUnion >> double: anObject [
	"This method was automatically generated"
	handle doubleAt: 1 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestUnion >> float [
	"This method was automatically generated"
	^handle floatAt: 1
]

{ #category : #'accessing structure variables' }
FFITestUnion >> float: anObject [
	"This method was automatically generated"
	handle floatAt: 1 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestUnion >> int64 [
	"This method was automatically generated"
	^handle signedLongLongAt: 1
]

{ #category : #'accessing structure variables' }
FFITestUnion >> int64: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: 1 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestUnion >> long [
	"This method was automatically generated"
	^handle platformLongAt: 1
]

{ #category : #'accessing structure variables' }
FFITestUnion >> long: anObject [
	"This method was automatically generated"
	^handle platformLongAt: 1 put: anObject
]

{ #category : #'accessing structure variables' }
FFITestUnion >> short [
	"This method was automatically generated"
	^handle signedShortAt: 1
]

{ #category : #'accessing structure variables' }
FFITestUnion >> short: anObject [
	"This method was automatically generated"
	handle signedShortAt: 1 put: anObject
]
