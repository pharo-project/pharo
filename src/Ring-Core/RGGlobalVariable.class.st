"
A global variable definition
"
Class {
	#name : #RGGlobalVariable,
	#superclass : #RGObject,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'managing container' }
RGGlobalVariable >> addoptToParentStub [

	super addoptToParentStub.
	self parent addGlobalVariable: self.
]

{ #category : #'managing container' }
RGGlobalVariable >> defaultParentStub [

	^ self defaultEnvironmentStub
]

{ #category : #testing }
RGGlobalVariable >> isGlobalVariable [

	^true
]
