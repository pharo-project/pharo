Class {
	#name : #RBRemoveSharedVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRemoveSharedVariableRefactoring class >> model: aNamespace remove: variable from: class [

	^ self model: aNamespace variable: variable class: class
]

{ #category : #removing }
RBRemoveSharedVariableRefactoring class >> remove: variable from: class [

	^ self variable: variable class: class
]

{ #category : #preconditions }
RBRemoveSharedVariableRefactoring >> applicabilityPreconditions [

	^ ( RBCondition isMetaclass: class ) not
		& ( RBCondition definesClassVariable: variableName in: class )

]

{ #category : #preconditions }
RBRemoveSharedVariableRefactoring >> breakingChangeConditions [
	
	^ (RBCondition
		   hierarchyOf: class
		   referencesSharedVariable: variableName) not
]

{ #category : #actions }
RBRemoveSharedVariableRefactoring >> generateChanges [
	
	self applicabilityPreconditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityPreconditions errorString ].
	self breakingChangeConditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangeConditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : #preconditions }
RBRemoveSharedVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangeConditions 
]

{ #category : #transforming }
RBRemoveSharedVariableRefactoring >> privateTransform [
	class removeClassVariable: variableName
]

{ #category : #accessing }
RBRemoveSharedVariableRefactoring >> refactoredClass [

	^ class
]
