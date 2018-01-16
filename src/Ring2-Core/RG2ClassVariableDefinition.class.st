"
A class variable definition
"
Class {
	#name : #RG2ClassVariableDefinition,
	#superclass : #RG2VariableDefinition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2ClassVariableDefinition >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent addClassVariable: self.
]

{ #category : #'managing container' }
RG2ClassVariableDefinition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #printing }
RG2ClassVariableDefinition >> definitionString [

	"TODO: special class variables?"

	^ self name printString

]

{ #category : #testing }
RG2ClassVariableDefinition >> isClassVariable [

	^true
]
