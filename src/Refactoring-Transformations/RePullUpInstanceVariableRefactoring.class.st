"
I am a refactoring for moving an instance variable up **to** the superclass of the class that defines the instance variable.

"
Class {
	#name : 'RePullUpInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RePullUpInstanceVariableRefactoring >> applicabilityPreconditions [

	"^ { (RBCondition withBlock: [
		   (class hierarchyDefinesInstanceVariable: variableName) ifFalse: [
			   self refactoringError: 'No subclass defines ' , variableName ].
		   true ]) }"
	
	"we will have to handle multiple variables"
"	^ { ReHierarchyDefinesInstanceVariableCondition new instanceVariables: { variableName } ; yourself}"
	
	^ { (ReHierarchyDefinesInstanceVariableCondition new class: class; instanceVariables: { variableName }; yourself) }
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

{ #category : 'transforming' }
RePullUpInstanceVariableRefactoring >> privateTransform [
	"Remove all the variables in the hierarchy below the classes and define one on the top."
	
	class allSubclasses do:
			[:each |
			(each directlyDefinesInstanceVariable: variableName)
				ifTrue: [each removeInstanceVariable: variableName]].
	class addInstanceVariable: variableName
]

{ #category : 'preconditions' }
RePullUpInstanceVariableRefactoring >> subclassesNotDefiningTheVariable [
	
	^ class allSubclasses 
			reject: [ :each | each directlyDefinesInstanceVariable: variableName ]
			 
]
