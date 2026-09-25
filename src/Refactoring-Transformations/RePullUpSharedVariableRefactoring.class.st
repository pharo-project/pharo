"
I am a refactoring for moving a class variable up to the superclass.
"
Class {
	#name : 'RePullUpSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RePullUpSharedVariableRefactoring >> applicabilityPreconditions [
	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
RePullUpSharedVariableRefactoring >> breakingChangePreconditions [

	^ { (RBCondition withBlock: [
		   (class subclasses anySatisfy: [ :each |
			    (each directlyDefinesClassVariable: variableName) not ])
			   ifTrue: [
				   self refactoringWarning:
					   'Not all subclasses have a shared variable named.<n> Do you want pull up this variable anyway?'
					   , variableName , '.' ].
		   true ]) }
]

{ #category : 'initialization' }
RePullUpSharedVariableRefactoring >> model: aRBNamespace [
	super model: aRBNamespace.
	transformation ifNotNil: [ transformation model: aRBNamespace ]
]

{ #category : 'transforming' }
RePullUpSharedVariableRefactoring >> privateTransform [
	^ transformation privateTransform
]

{ #category : 'initialization' }
RePullUpSharedVariableRefactoring >> variable: aVarName class: aClassName [
	super variable: aVarName class: aClassName.
	transformation := RBPullUpVariableTransformation
		model: self model
		classVariable: aVarName
		class: aClassName
]
