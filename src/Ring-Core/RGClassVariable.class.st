"
A class variable definition
"
Class {
	#name : #RGClassVariable,
	#superclass : #RGVariable,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'managing container' }
RGClassVariable >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent addClassVariable: self.
]

{ #category : #'managing container' }
RGClassVariable >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #printing }
RGClassVariable >> definitionString [

	"TODO: special class variables?"

	^ self name printString

]

{ #category : #testing }
RGClassVariable >> isClassVariable [

	^true
]
