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
RBPullUpClassVariableRefactoring >> preconditions [
	^(RBCondition isMetaclass: class) not
]

{ #category : 'transforming' }
RBPullUpClassVariableRefactoring >> privateTransform [
	| subclass |
	subclass := self subclassDefiningVariable.
	subclass removeClassVariable: variableName.
	class addClassVariable: variableName
]

{ #category : 'private - accessing' }
RBPullUpClassVariableRefactoring >> subclassDefiningVariable [
	| subclasses |
	subclasses := class allSubclasses
		select: [ :each | each isMeta not and: [ each directlyDefinesClassVariable: variableName ] ].
	subclasses ifEmpty: [ self refactoringError: 'Could not find a class defining ' , variableName ].
	subclasses size > 1
		ifTrue: [ self refactoringError: 'Multiple subclasses define ' , variableName ].
	^ subclasses asArray first
]
