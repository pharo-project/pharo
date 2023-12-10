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

	^ ( RBIsNotAMetaclass class: class )
		& ( RBDirectlyDefinesSharedVariableCondition class: class sharedVariables: {variableName} )

]

{ #category : 'preconditions' }
RBRemoveSharedVariableRefactoring >> breakingChangePreconditions [
	
	^ breakingChangePreconditions ifNil: [  breakingChangePreconditions := 
		(RBSharedVariableHasReferences new 
			hierarchyOf: class referencesSharedVariable: variableName) not ]
	
]

{ #category : 'executing' }
RBRemoveSharedVariableRefactoring >> generateChanges [

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
