"
A simple refactoring that removes an instance variable. 
The variable should not be used obviously.
"
Class {
	#name : 'ReRemoveInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'applicabilityPreconditions',
		'breakingChangePreconditions'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'displaying' }
ReRemoveInstanceVariableRefactoring class >> basicMenuItemString [

	^ 'Remove Instance Variable'
]

{ #category : 'instance creation' }
ReRemoveInstanceVariableRefactoring class >> model: aNamespace remove: variable from: class [
	^ self model: aNamespace variable: variable class: class
]

{ #category : 'instance creation' }
ReRemoveInstanceVariableRefactoring class >> remove: variable from: class [
	^ self variable: variable class: class
]

{ #category : 'preconditions' }
ReRemoveInstanceVariableRefactoring >> applicabilityPreconditions [
	"we store it because the violators are computed during check so we should not recreate it each time."

	^ applicabilityPreconditions ifNil: [
		  applicabilityPreconditions := { 
				(ReDirectlyDefinesInstanceVariableCondition
					classNamed: class name
					inModel: self model
					instanceVariables: { variableName }) } ]
]

{ #category : 'preconditions' }
ReRemoveInstanceVariableRefactoring >> breakingChangePreconditions [ 

	^ breakingChangePreconditions ifNil: [  breakingChangePreconditions := 
		(ReInstanceVariableHasReferences new 
			hierarchyOf: class referencesInstanceVariable: variableName) not ]
	
]

{ #category : 'executing' }
ReRemoveInstanceVariableRefactoring >> generateChanges [
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
ReRemoveInstanceVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'printing' }
ReRemoveInstanceVariableRefactoring >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '( Class: ', class asString. 
	aStream 
		nextPutAll: ' , ' ;
		nextPutAll: variableName asString;
		nextPutAll: ')' 
	
]

{ #category : 'transforming' }
ReRemoveInstanceVariableRefactoring >> privateTransform [

	class removeInstanceVariable: variableName
]

{ #category : 'accessing' }
ReRemoveInstanceVariableRefactoring >> refactoredClass [
	^ class
]
