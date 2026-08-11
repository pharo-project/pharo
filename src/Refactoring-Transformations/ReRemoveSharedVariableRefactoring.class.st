"
A simple refactoring to remove a shared variable.
"
Class {
	#name : 'ReRemoveSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'displaying' }
ReRemoveSharedVariableRefactoring class >> basicMenuItemString [

	^ 'Remove Shared Variable'
]

{ #category : 'instance creation' }
ReRemoveSharedVariableRefactoring class >> model: aNamespace remove: variable from: class [

	^ self model: aNamespace variable: variable class: class
]

{ #category : 'removing' }
ReRemoveSharedVariableRefactoring class >> remove: variable from: class [

	^ self variable: variable class: class
]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> applicabilityPreconditions [

	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> breakingChangePreconditions [

	^ { (ReSharedVariableHasReferences new
		   hierarchyOf: class
		   referencesSharedVariable: variableName) not }
]

{ #category : 'accessing' }
ReRemoveSharedVariableRefactoring >> model: aRBNamespace [

	super model: aRBNamespace.
	transformation ifNotNil: [ transformation model: aRBNamespace ]
]

{ #category : 'preconditions' }
ReRemoveSharedVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
ReRemoveSharedVariableRefactoring >> privateTransform [

	transformation privateTransform
]

{ #category : 'accessing' }
ReRemoveSharedVariableRefactoring >> refactoredClass [

	^ class
]

{ #category : 'initialization' }
ReRemoveSharedVariableRefactoring >> variable: aVarName class: aClassName [

	super variable: aVarName class: aClassName.
	transformation := RBRemoveVariableTransformation
				  model: self model
				  classVariable: aVarName
				  class: aClassName
]
