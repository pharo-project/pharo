"
Adds a variable in a class. It is necessary to indicate whether the variable is an instance or class variable.

Usage:
| transformation |
transformation := (RBAddVariableTransformation 
	variable: 'asdf'
	class: #RBVariableTransformation
	classVariable: false)
	transform. 
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the variable name should not be a class name
- the class (and any class in the hierarchy) should not have a variable with the same name
"
Class {
	#name : #RBAddVariableTransformation,
	#superclass : #RBVariableTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #preconditions }
RBAddVariableTransformation >> preconditions [

	class := self definingClass.
	
	^ (isClassVariable 
		ifTrue: [ (RBCondition isMetaclass: class)
					& (RBCondition isValidClassVarName: variableName for: class) ]
		ifFalse: [ (RBCondition isMetaclass: class) not
					& (RBCondition isValidInstanceVariableName: variableName for: class) ])
		& (RBCondition hierarchyOf: class definesVariable: variableName asString) not 
		& (RBCondition isGlobal: variableName in: self model) not
]

{ #category : #executing }
RBAddVariableTransformation >> privateTransform [
	
	isClassVariable
		ifTrue: [ self definingClass theNonMetaClass addClassVariable: variableName ]
		ifFalse: [ self definingClass addInstanceVariable: variableName ]
]
