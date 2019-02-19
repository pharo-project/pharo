"
Creates accessors for a variable in a class, then replaces all the direct accesses to this variable by invocation to the accessors.

Usage:
transformation := (RBProtectVariableTransformation
	instanceVariable: 'class'
	class: #RBTransformationRuleTest)
	transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
overriden from RBAddAccessorsForClassTransformation and RBRemoveDirectAccessToVariableTransformation
"
Class {
	#name : #RBProtectVariableTransformation,
	#superclass : #RBInHierarchyVariableTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #executing }
RBProtectVariableTransformation >> buildTransformations [

	^ OrderedCollection
		with: (RBAddVariableAccessorTransformation
				model: self model
				variable: variableName asString
				class: className
				classVariable: isClassVariable)
		with: (RBRemoveDirectAccessToVariableTransformation
				model: self model
				variable: variableName asString
				class: className
				classVariable: isClassVariable)
]
