"
I am a refactoring for merge an instance variable into another.

I replace an instance variable by other, in all methods refering to this variable and rename the old accessors, then if the instance variable renamed is directly defined in class it is removed.

My precondition verifies that the new variable is a defined instance variable in class.

Example
----------------------------
Script
```
(RBMergeInstanceVariableIntoAnother rename: 'x' to: 'y' in: Foo) execute.
```

Before refactoring:
```
Class Foo -> inst vars: x, y 

Foo >> foobar
	^ x 

Foo >> foo
	^ x + y 
```
After refactoring merging X into Y
```
Class Foo -> inst vars: y 

Foo >> foobar
	^ y

Foo >> foo 
	^ y + y
```
"
Class {
	#name : 'RBMergeInstanceVariableIntoAnother',
	#superclass : 'RBRenameInstanceVariableRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBMergeInstanceVariableIntoAnother >> applicabilityPreconditions [

	^ {
		  (RBCondition withBlock: [
			   variableName = newName ifTrue: [
				   self refactoringError: 'The variable merged must be different' ].
			   true ]).
		  (ReDefinesInstanceVariableCondition
			   classNamed: class name
			   inModel: self model
			   instanceVariables: { variableName }).
		  (ReDefinesInstanceVariableCondition
			   classNamed: class name
			   inModel: self model
			   instanceVariables: { newName }) }
]

{ #category : 'transforming' }
RBMergeInstanceVariableIntoAnother >> privateTransform [
	renameAccessors ifTrue: [
		self removeOldAccessors
	].

	self renameVariable.

	renameAccessors ifFalse: [ ^ self ].
	self addNewAccessors.
	self renameAccessorsReferences
]

{ #category : 'transforming' }
RBMergeInstanceVariableIntoAnother >> renameVariable [
	model
		renameInstanceVariable: variableName
		to: newName
		in: class
		around: [ self renameReferences ]
]
