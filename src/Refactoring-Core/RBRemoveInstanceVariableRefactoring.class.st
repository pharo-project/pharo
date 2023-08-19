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
RBRemoveInstanceVariableRefactoring >> applicabilityPreconditions [

	^ RBCondition directlyDefinesInstanceVariable: variableName asString in: class
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> breakingChangePreconditions [ 

	^ (RBCondition hierarchyOf: class referencesInstanceVariable: variableName) not
	
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> generateChanges [
	
	self applicabilityPreconditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityPreconditions errorString ].
	self breakingChangePreconditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangePreconditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : #preconditions }
RBRemoveInstanceVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : #transforming }
RBRemoveInstanceVariableRefactoring >> privateTransform [
	
	
	class removeInstanceVariable: variableName
]

{ #category : #accessing }
RBRemoveInstanceVariableRefactoring >> refactoredClass [
	^ class
]
