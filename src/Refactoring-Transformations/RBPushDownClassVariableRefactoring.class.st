"
I am a refactoring for moving a class variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RBPushDownClassVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> applicabilityPreconditions [
	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> breakingChangePreconditions [

    ^ { self preconditionHasNoReferences }
]

{ #category : 'initialization' }
RBPushDownClassVariableRefactoring >> model: aRBNamespace [
	super model: aRBNamespace.
	transformation ifNotNil: [ transformation model: aRBNamespace ]
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> preconditionHasNoReferences [

    ^ (ReSharedVariableHasReferencesInDefiningClass
            new
            class: class
            referencesToSharedVariable: variableName) not
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> preconditionHasSubclass [

    ^ (ReClassesHaveNoSubclassesCondition new
            classes: { class }) not
]

{ #category : 'transforming' }
RBPushDownClassVariableRefactoring >> privateTransform [
	^ transformation privateTransform
]

{ #category : 'initialization' }
RBPushDownClassVariableRefactoring >> variable: aVarName class: aClassName [
	super variable: aVarName class: aClassName.
	transformation := RBPushDownVariableTransformation
		model: self model
		classVariable: aVarName
		class: aClassName
]
