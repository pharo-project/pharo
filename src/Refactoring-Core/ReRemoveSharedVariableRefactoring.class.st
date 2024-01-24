"
A simple refactoring to remove a shared variable.
"
Class {
	#name : 'ReRemoveSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'breakingChangePreconditions'
	],
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

	^ ( ReIsNotAMetaclass class: class )
		& ( ReDirectlyDefinesSharedVariableCondition class: class sharedVariables: {variableName} )

]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> breakingChangePreconditions [

	^ breakingChangePreconditions ifNil: [  breakingChangePreconditions := 
		(ReSharedVariableHasReferences new 
			hierarchyOf: class referencesSharedVariable: variableName) not ]
	
]

{ #category : 'executing' }
ReRemoveSharedVariableRefactoring >> generateChanges [
	"Prepare, check the preconditions, and perform the actual transformation (i.e., creating a list of changes that can be accessible using the changes message). This method should not be invoked from drivers since drivers usually check in finer grained mode the preconditions - and that this stage they already checked the preconditions."
	
	self prepareForExecution.

	self applicabilityPreconditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityPreconditions errorString ].
	self breakingChangePreconditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangePreconditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : 'transforming' }
ReRemoveSharedVariableRefactoring >> privateTransform [
	class removeClassVariable: variableName
]

{ #category : 'accessing' }
ReRemoveSharedVariableRefactoring >> refactoredClass [

	^ class
]
