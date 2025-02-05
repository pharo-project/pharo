Class {
	#name : 'ReConvertTemporaryToInstanceVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable',
		'selector'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'instance creation' }
ReConvertTemporaryToInstanceVariableDriver >> class: aClass selector: aSelector variable: aVariable [

	variable := aVariable.
	selector := aSelector.
	class := aClass
]

{ #category : 'resources' }
ReConvertTemporaryToInstanceVariableDriver >> configureRefactoring [

	refactoring := RBTemporaryToInstanceVariableTransformation class: class selector: selector variable: variable.
	refactoring prepareForExecution
]

{ #category : 'execution' }
ReConvertTemporaryToInstanceVariableDriver >> runRefactoring [

	self configureRefactoring.
	"TODO"
	self applyChanges 
]

{ #category : 'instance creation' }
ReConvertTemporaryToInstanceVariableDriver >> scopes: aScope class: aClass selector: aSelector variable: aVariable [

	scopes := aScope.
	variable := aVariable.
	selector := aSelector.
	class := aClass
]
