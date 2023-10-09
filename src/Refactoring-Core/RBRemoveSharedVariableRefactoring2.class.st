Class {
	#name : 'RBRemoveSharedVariableRefactoring2',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRemoveSharedVariableRefactoring2 class >> model: aNamespace remove: variable from: class [

	^ self model: aNamespace variable: variable class: class
]

{ #category : 'removing' }
RBRemoveSharedVariableRefactoring2 class >> remove: variable from: class [

	^ self variable: variable class: class
]

{ #category : 'preconditions' }
RBRemoveSharedVariableRefactoring2 >> applicabilityPreconditions [

	^ ( RBCondition isMetaclass: class ) not
		& ( RBCondition definesClassVariable: variableName in: class )

]

{ #category : 'preconditions' }
RBRemoveSharedVariableRefactoring2 >> breakingChangePreconditions [
	
	^ (RBCondition
		   hierarchyOf: class
		   referencesSharedVariable: variableName) not
]

{ #category : 'executing' }
RBRemoveSharedVariableRefactoring2 >> generateChanges [

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
RBRemoveSharedVariableRefactoring2 >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBRemoveSharedVariableRefactoring2 >> privateTransform [
	class removeClassVariable: variableName
]

{ #category : 'accessing' }
RBRemoveSharedVariableRefactoring2 >> refactoredClass [

	^ class
]
