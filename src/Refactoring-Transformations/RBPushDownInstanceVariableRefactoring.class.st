"
I am a refactoring for moving a instance variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RBPushDownInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> applicabilityPreconditions [
	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> breakingChangePreconditions [

	^ { self preconditionReferencesInstanceVariable }
]

{ #category : 'initialization' }
RBPushDownInstanceVariableRefactoring >> model: aRBNamespace [
	super model: aRBNamespace.
	transformation ifNotNil: [ transformation model: aRBNamespace ]
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> preconditionReferencesInstanceVariable [

	^ ReReferencesInstanceVariableCondition class: class inModel: self model instanceVariables: { variableName }
]

{ #category : 'preconditions' }
RBPushDownInstanceVariableRefactoring >> preconditions [ 

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBPushDownInstanceVariableRefactoring >> privateTransform [
	^ transformation privateTransform
]

{ #category : 'initialization' }
RBPushDownInstanceVariableRefactoring >> variable: aVarName class: aClassName [
	super variable: aVarName class: aClassName.
	transformation := RBPushDownVariableTransformation
		model: self model
		instanceVariable: aVarName
		class: aClassName
]
