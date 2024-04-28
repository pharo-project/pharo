"
A simple refactoring to remove a shared variable.
"
Class {
	#name : 'ReRemoveSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'displaying' }
ReRemoveSharedVariableRefactoring class >> basicMenuItemString [

	^ 'Remove Shared Variable'
]

{ #category : 'instance creation' }
ReRemoveSharedVariableRefactoring class >> model: aNamespace remove: variable from: class [

	^ self model: aNamespace variable: variable class: class
]

{ #category : 'removing' }
ReRemoveSharedVariableRefactoring class >> remove: variable from: class [

	^ self variable: variable class: class
]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (ReIsNotAMetaclass class: class).
		  (ReDirectlyDefinesSharedVariableCondition
			   class: class
			   sharedVariables: { variableName }) }
]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> breakingChangePreconditions [

	^ { (ReSharedVariableHasReferences new
		   hierarchyOf: class
		   referencesSharedVariable: variableName) not }
]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
ReRemoveSharedVariableRefactoring >> privateTransform [
	class removeClassVariable: variableName
]

{ #category : 'accessing' }
ReRemoveSharedVariableRefactoring >> refactoredClass [

	^ class
]
