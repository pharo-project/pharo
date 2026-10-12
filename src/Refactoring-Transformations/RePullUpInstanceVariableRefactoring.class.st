"
I am a refactoring for moving an instance variable up **to** the superclass of the class that defines the instance variable.

"
Class {
	#name : 'RePullUpInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'transformation'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RePullUpInstanceVariableRefactoring >> applicabilityPreconditions [
	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
RePullUpInstanceVariableRefactoring >> breakingChangePreconditions [

	^ { (RBCondition withBlock: [
			   (class subclasses anySatisfy: [ :each | (each directlyDefinesInstanceVariable: variableName) not ]) ifTrue: [
					   self refactoringWarning:
						   ('Not all subclasses have an instance variable named {1}. Do you want to pull up this variable anyway?' format:
							    { variableName }) ].
			   true ]) }
]

{ #category : 'initialization' }
RePullUpInstanceVariableRefactoring >> model: aRBNamespace [
	super model: aRBNamespace.
	transformation ifNotNil: [ transformation model: aRBNamespace ]
]

{ #category : 'transforming' }
RePullUpInstanceVariableRefactoring >> privateTransform [
	^ transformation privateTransform
]

{ #category : 'preconditions' }
RePullUpInstanceVariableRefactoring >> subclassesNotDefiningTheVariable [
	
	^ class allSubclasses 
			reject: [ :each | each directlyDefinesInstanceVariable: variableName ]
			 
]

{ #category : 'initialization' }
RePullUpInstanceVariableRefactoring >> variable: aVarName class: aClassName [
	super variable: aVarName class: aClassName.
	transformation := RBPullUpVariableTransformation
		model: self model
		instanceVariable: aVarName
		class: aClassName
]
