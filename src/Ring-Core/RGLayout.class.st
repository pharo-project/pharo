Class {
	#name : #RGLayout,
	#superclass : #RGObject,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'managing container' }
RGLayout >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent behaviorStrategy pvtLayout: self.
]

{ #category : #accessing }
RGLayout >> allSlots [

	^ {  }
]

{ #category : #'managing container' }
RGLayout >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #accessing }
RGLayout >> instVarNames [
	^ {}
]

{ #category : #'testing types' }
RGLayout >> isBitsLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isByteLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isCompiledMethodLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isEmptyLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isEphemeronLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isFixedLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isImmediateLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isLayout [

	^true
]

{ #category : #'testing types' }
RGLayout >> isObjectLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isPointerLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isVariableLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isWeakLayout [

	^ false
]

{ #category : #'testing types' }
RGLayout >> isWordLayout [

	^ false
]
