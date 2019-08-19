"
I am a refactoring for moving a class variable up to the superclass.
"
Class {
	#name : #RBPullUpClassVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBPullUpClassVariableRefactoring >> preconditions [
	^(RBCondition isMetaclass: class) not
]

{ #category : #'private-accessing' }
RBPullUpClassVariableRefactoring >> subclassDefiningVariable [
	| subclasses |
	subclasses := class allSubclasses 
		select: [ :each | each isMeta not and: [ each directlyDefinesClassVariable: variableName ] ].
	subclasses ifEmpty: [ self refactoringFailure: 'Could not find a class defining ' , variableName ].
	subclasses size > 1 
		ifTrue: [ self refactoringError: 'Multiple subclasses define ' , variableName ].
	^ subclasses asArray first
]

{ #category : #transforming }
RBPullUpClassVariableRefactoring >> transform [
	| subclass |
	subclass := self subclassDefiningVariable.
	subclass removeClassVariable: variableName.
	class addClassVariable: variableName
]
