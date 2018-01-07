"
A pool variable definition
"
Class {
	#name : #RG2PoolVariableDefinition,
	#superclass : #RG2VariableDefinition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2PoolVariableDefinition >> addoptToParentStub [

	super addoptToParentStub.

	self environment backend createUnresolvedClassGroupFor: self parent
]

{ #category : #'managing container' }
RG2PoolVariableDefinition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #testing }
RG2PoolVariableDefinition >> isPoolVariable [

	^true
]
