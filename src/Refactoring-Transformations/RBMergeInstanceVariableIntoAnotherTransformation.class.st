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

	conds := {
		         (RBCondition withBlock: [
			          variableName = newVariableName ifTrue: [
				          self refactoringError:
					          'The variable merged must be different' ].
			          true ]).
		         self isNotGlobal }.
	^ isClassVariable
		  ifTrue: [
			  conds , {
				  (RBCondition isMetaclass: class) not.
				  (RBCondition
					   definesClassVariable: newVariableName asString
					   in: class).
				  (RBCondition
					   definesClassVariable: variableName asString
					   in: class) } ]
		  ifFalse: [
			  conds , {
				  (RBCondition definesInstanceVariable: newVariableName in: class).
				  (RBCondition definesInstanceVariable: variableName in: class) } ]
]

{ #category : 'executing' }
RBMergeInstanceVariableIntoAnotherTransformation >> privateTransform [

	self renameReferences.
	isClassVariable
	ifTrue: [ self definingClass removeClassVariable: variableName ifAbsent: [ ^ self ] ]
	ifFalse: [ self definingClass removeInstanceVariable: variableName ifAbsent: [ ^ self ] ]
]
