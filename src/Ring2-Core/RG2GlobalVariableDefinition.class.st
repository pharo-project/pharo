"
A global variable definition
"
Class {
	#name : #RG2GlobalVariableDefinition,
	#superclass : #RG2Definition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2GlobalVariableDefinition >> addoptToParentStub [

	super addoptToParentStub.
	self parent addGlobalVariable: self.
]

{ #category : #'managing container' }
RG2GlobalVariableDefinition >> defaultParentStub [

	^ self defaultEnvironmentStub
]

{ #category : #testing }
RG2GlobalVariableDefinition >> isGlobalVariable [

	^true
]
