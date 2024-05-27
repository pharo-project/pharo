"
I am a refactoring for moving a class variable up to the superclass.
"
Class {
	#name : 'RBPullUpClassVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBPullUpClassVariableRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isMetaclass: class) not.
		  (RBCondition withBlock: [
			   (class hierarchyDefinesClassVariable: variableName) ifFalse: [
				   self refactoringError: 'No subclass defines ' , variableName ].
			   true ]) }
]

{ #category : 'preconditions' }
RBPullUpClassVariableRefactoring >> breakingChangePreconditions [

	^ { (RBCondition withBlock: [
		   (class subclasses anySatisfy: [ :each |
			    (each directlyDefinesClassVariable: variableName) not ])
			   ifTrue: [
				   self refactoringWarning:
					   'Not all subclasses have an class variable named.<n> Do you want pull up this variable anyway?'
					   , variableName , '.' ].
		   true ]) }
]

{ #category : 'transforming' }
RBPullUpClassVariableRefactoring >> privateTransform [

	class allSubclasses do:
		[:each |
		(each directlyDefinesClassVariable: variableName)
			ifTrue: [each removeClassVariable: variableName]].
	class addClassVariable: variableName
]
