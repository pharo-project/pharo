"
A simple refactoring to remove a shared variable.
"
Class {
	#name : 'RBRemoveSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'breakingChangePreconditions'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'displaying' }
RBRemoveSharedVariableRefactoring class >> basicMenuItemString [

	^ 'Remove Shared Variable'
]

{ #category : 'instance creation' }
RBRemoveSharedVariableRefactoring class >> model: aNamespace remove: variable from: class [

	^ self model: aNamespace variable: variable class: class
]

{ #category : 'removing' }
RBRemoveSharedVariableRefactoring class >> remove: variable from: class [

	^ self variable: variable class: class
]

{ #category : 'preconditions' }
RBRemoveSharedVariableRefactoring >> applicabilityPreconditions [

	^ ( ReIsNotAMetaclass class: class )
		& ( ReDirectlyDefinesSharedVariableCondition class: class sharedVariables: {variableName} )

]

{ #category : 'preconditions' }
RBRemoveSharedVariableRefactoring >> breakingChangePreconditions [

	^ breakingChangePreconditions ifNil: [  breakingChangePreconditions := 
		(ReSharedVariableHasReferences new 
			hierarchyOf: class referencesSharedVariable: variableName) not ]
	
]

{ #category : 'executing' }
RBRemoveSharedVariableRefactoring >> generateChanges [
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

{ #category : 'preconditions' }
RBRemoveSharedVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBRemoveSharedVariableRefactoring >> privateTransform [
	class removeClassVariable: variableName
]

{ #category : 'accessing' }
RBRemoveSharedVariableRefactoring >> refactoredClass [

	^ class
]
