Class {
	#name : #RBRemoveInstanceVariableRefactoring2,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRemoveInstanceVariableRefactoring2 class >> model: aNamespace remove: variable from: class [
	^ self model: aNamespace variable: variable class: class
]

{ #category : #'instance creation' }
RBRemoveInstanceVariableRefactoring2 class >> remove: variable from: class [
	^ self variable: variable class: class
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring2 >> applicabilityConditions [

	^ RBCondition directlyDefinesInstanceVariable: variableName asString in: class
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring2 >> breakingChangeConditions [ 

	^ (RBCondition hierarchyOf: class referencesInstanceVariable: variableName) not
	
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring2 >> generateChanges [
	self halt.
	self applicabilityConditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityConditions errorString ].
	self halt.
	self breakingChangeConditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangeConditions errorString ].

	self transform.
	^ self changes
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring2 >> preconditions [

	^ self applicabilityConditions & self breakingChangeConditions 
]

{ #category : #transforming }
RBRemoveInstanceVariableRefactoring2 >> transform [
	class removeInstanceVariable: variableName
]
