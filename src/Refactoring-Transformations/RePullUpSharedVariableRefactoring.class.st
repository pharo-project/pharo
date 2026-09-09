"
I am a refactoring for moving a class variable up to the superclass.
"
Class {
	#name : 'RePullUpSharedVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
RePullUpSharedVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isMetaclass: class) not.
		  (ReHierarchyDefinesSharedVariableCondition new
			   class: class;
			   sharedVariables: { variableName };
			   yourself) }
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

{ #category : 'transforming' }
RePullUpSharedVariableRefactoring >> privateTransform [
	"Remove all the variables in the hierarchy below the classes and define one on the top."
	
	class allSubclasses do:
		[:each |
		(each directlyDefinesClassVariable: variableName)
			ifTrue: [each removeClassVariable: variableName]].
	class addClassVariable: variableName
]
