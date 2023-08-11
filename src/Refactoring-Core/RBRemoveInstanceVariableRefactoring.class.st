Class {
	#name : #RBRemoveInstanceVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRemoveInstanceVariableRefactoring class >> model: aNamespace remove: variable from: class [
	^ self model: aNamespace variable: variable class: class
]

{ #category : #'instance creation' }
RBRemoveInstanceVariableRefactoring class >> remove: variable from: class [
	^ self variable: variable class: class
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> applicabilityConditions [

	^ RBCondition directlyDefinesInstanceVariable: variableName asString in: class
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> breakingChangeConditions [ 

	^ (RBCondition hierarchyOf: class referencesInstanceVariable: variableName) not
	
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> generateChanges [
	
	self applicabilityConditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityConditions errorString ].
	self breakingChangeConditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangeConditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> preconditions [

	^ self applicabilityConditions & self breakingChangeConditions 
]

{ #category : #transforming }
RBRemoveInstanceVariableRefactoring >> privateTransform [
	
	
	class removeInstanceVariable: variableName
]

{ #category : #accessing }
RBRemoveInstanceVariableRefactoring >> refactoredClass [
	^ class
]
