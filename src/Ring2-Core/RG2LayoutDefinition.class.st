Class {
	#name : #RG2LayoutDefinition,
	#superclass : #RG2Definition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2LayoutDefinition >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent pvtLayout: self.
]

{ #category : #accessing }
RG2LayoutDefinition >> allSlots [

	^ {  }
]

{ #category : #'managing container' }
RG2LayoutDefinition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #accessing }
RG2LayoutDefinition >> instVarNames [
	^ {}
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isBitsLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isByteLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isCompiledMethodLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isEmptyLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isEphemeronLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isFixedLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isImmediateLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isLayout [

	^true
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isObjectLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isPointerLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isVariableLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isWeakLayout [

	^ false
]

{ #category : #'testing types' }
RG2LayoutDefinition >> isWordLayout [

	^ false
]
