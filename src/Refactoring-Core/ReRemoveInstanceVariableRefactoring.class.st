"
A simple refactoring that removes an instance variable. 
The variable should not be used obviously.
"
Class {
	#name : 'ReRemoveInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
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

	^ { (ReDirectlyDefinesInstanceVariableCondition
		   classNamed: class name
		   inModel: self model
		   instanceVariables: { variableName }) }
]

{ #category : 'preconditions' }
ReRemoveInstanceVariableRefactoring >> breakingChangePreconditions [

	^ { (ReInstanceVariableHasReferences new
		   hierarchyOf: class
		   referencesInstanceVariable: variableName) not }
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
