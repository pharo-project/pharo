"
A simple refactoring that removes an instance variable. 
The variable should not be used obviously.
"
Class {
	#name : 'ReRemoveInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
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

	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
ReRemoveInstanceVariableRefactoring >> breakingChangePreconditions [

	^ { self preconditionHasReferences }
]

{ #category : 'accessing' }
ReRemoveInstanceVariableRefactoring >> model: aRBNamespace [

	super model: aRBNamespace.
	transformation ifNotNil: [ transformation model: aRBNamespace ]
]

{ #category : 'preconditions' }
ReRemoveInstanceVariableRefactoring >> preconditionHasReferences [

	^ (ReInstanceVariableHasNoReferences new hierarchyOf: class referencesInstanceVariable: variableName) 
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

	transformation privateTransform
]

{ #category : 'accessing' }
ReRemoveInstanceVariableRefactoring >> refactoredClass [
	^ class
]

{ #category : 'initialization' }
ReRemoveInstanceVariableRefactoring >> variable: aVarName class: aClassName [

	super variable: aVarName class: aClassName.
	transformation := RBRemoveVariableTransformation
				  model: self model
				  instanceVariable: aVarName
				  class: aClassName
]
