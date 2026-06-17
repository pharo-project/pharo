"
I am a refactoring for moving a instance variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RBPushDownInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> applicabilityPreconditions [

	^ { (RBCondition definesInstanceVariable: variableName in: class) }
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> breakingChangePreconditions [

	^ { self preconditionReferencesInstanceVariable }
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> preconditionReferencesInstanceVariable [

	^ ReReferencesInstanceVariableCondition class: class inModel: self model instanceVariables: { variableName }
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> preconditions [ 

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBPushDownInstanceVariableRefactoring >> privateTransform [
	class removeInstanceVariable: variableName.
	class subclasses do: [:each | each addInstanceVariable: variableName]
]
