Class {
	#name : 'RBMergeInstanceVariableIntoAnotherTransformation',
	#superclass : 'RBRenameVariableTransformation',
	#category : 'Refactoring-Transformations-Model-Migrated-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Migrated-Unused'
}

{ #category : 'preconditions' }
RBMergeInstanceVariableIntoAnotherTransformation >> applicabilityPreconditions [

	| conds |
	class := self model classObjectFor: className.

	conds := { (RBCondition withBlock: [
		          variableName = newVariableName ifTrue: [
			          self refactoringError:
				          'The variable merged must be different' ].
		          true ]) }.
	isClassVariable
		ifTrue: [
			conds
				add: (RBCondition isMetaclass: class) not;
				add:
					(RBCondition
						 definesClassVariable: newVariableName asString
						 in: class);
				add:
					(RBCondition
						 definesClassVariable: variableName asString
						 in: class) ]
		ifFalse: [
			conds
				add:
					(RBCondition definesInstanceVariable: newVariableName in: class);
				add: (RBCondition definesInstanceVariable: variableName in: class) ].
	conds add: self isNotGlobal.
	^ conds
]

{ #category : 'executing' }
RBMergeInstanceVariableIntoAnotherTransformation >> privateTransform [

	self renameReferences.
	isClassVariable
	ifTrue: [ self definingClass removeClassVariable: variableName ifAbsent: [ ^ self ] ]
	ifFalse: [ self definingClass removeInstanceVariable: variableName ifAbsent: [ ^ self ] ]
]
