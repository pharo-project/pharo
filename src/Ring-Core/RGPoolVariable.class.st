"
A pool variable definition
"
Class {
	#name : #RGPoolVariable,
	#superclass : #RGVariable,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'managing container' }
RGPoolVariable >> addoptToParentStub [

	super addoptToParentStub.

	self environment backend createUnresolvedClassGroupFor: self parent
]

{ #category : #'managing container' }
RGPoolVariable >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #testing }
RGPoolVariable >> isPoolVariable [

	^true
]
