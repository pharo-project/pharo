"
I am a refactoring for merge an instance variable into another.

I rename the instance variable in the class definition, in all methods refering to this variable and rename the old accessors, then if the instance variable renamed is directly defined in class it is removed.

My precondition verifies that the new variable is a defined instance variable in class.

"
Class {
	#name : #RBMergeInstanceVariableIntoAnother,
	#superclass : #RBRenameInstanceVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBMergeInstanceVariableIntoAnother >> preconditions [
	^(RBCondition definesInstanceVariable: variableName in: class) 
			& (RBCondition definesInstanceVariable: newName in: class) 
]

{ #category : #transforming }
RBMergeInstanceVariableIntoAnother >> removeInstVar [
	(class directlyDefinesInstanceVariable: variableName) ifTrue: [ class removeInstanceVariable: variableName ]
]

{ #category : #transforming }
RBMergeInstanceVariableIntoAnother >> renameVariable [ 
	model 
		renameInstanceVariable: variableName
		to: newName
		in: class
		around: [ self renameReferences ].
]

{ #category : #transforming }
RBMergeInstanceVariableIntoAnother >> transform [
	renameAccessors ifTrue: [ 
		self removeOldAccessors 
	].
	
	self renameVariable.
	
	self removeInstVar.
	
	renameAccessors ifFalse: [ ^ self ].
	self addNewAccessors.
	self renameAccessorsReferences.
]
