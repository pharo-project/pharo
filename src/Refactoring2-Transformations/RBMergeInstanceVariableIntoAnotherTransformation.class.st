Class {
	#name : #RBMergeInstanceVariableIntoAnotherTransformation,
	#superclass : #RBRenameVariableTransformation,
	#category : #'Refactoring2-Transformations-Model-Migrated-Unused'
}

{ #category : #preconditions }
RBMergeInstanceVariableIntoAnotherTransformation >> preconditions [

	class := self model classObjectFor: className.

	^ (RBCondition
		withBlock: [ (variableName = newVariableName)
			ifTrue: [ self refactoringFailure: 'The variable merged must be different' ].
		true ])
	& (isClassVariable
		ifTrue: [ (RBCondition isMetaclass: class) not
					& (RBCondition definesClassVariable: newVariableName asString in: class)
					& (RBCondition definesClassVariable: variableName asString in: class) ]
		ifFalse: [ (RBCondition definesInstanceVariable: newVariableName in: class)
					& (RBCondition definesInstanceVariable: variableName in: class) ])
		& (RBCondition isGlobal: newVariableName in: self model) not
]

{ #category : #executing }
RBMergeInstanceVariableIntoAnotherTransformation >> privateTransform [

	self renameReferences.
	isClassVariable
	ifTrue: [ self definingClass removeClassVariable: variableName ifAbsent: [ ^ self ] ]
	ifFalse: [ self definingClass removeInstanceVariable: variableName ifAbsent: [ ^ self ] ]
]
