"
Adds a variable in a class. It is necessary to indicate whether the variable is an instance or class variable.

Usage:
```
| transformation |
transformation := (RBAddVariableTransformation 
	variable: 'asdf'
	class: #RBVariableTransformation
	classVariable: false)
	generateChanges. 
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the variable name should not be a class name
- the class (and any class in the hierarchy) should not have a variable with the same name
"
Class {
	#name : 'RBAddVariableTransformation',
	#superclass : 'RBVariableTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBAddVariableTransformation >> applicabilityPreconditions [

	| conds |
	class := self definingClass.
	conds := isClassVariable
		         ifTrue: [
			         {
				         (RBCondition isMetaclass: class) not.
				         (RBCondition isValidClassVarName: variableName for: class) } ]
		         ifFalse: [
			         { (RBCondition
				          isValidInstanceVariableName: variableName
				          for: class) } ].

	^ conds , {
		self isNotGlobal.
		(RBCondition
			 hierarchyOf: class
			 definesVariable: variableName asString) not }
]

{ #category : 'preconditions' }
RBAddVariableTransformation >> isNotGlobal [

	^ (RBCondition isGlobal: variableName in: self model) not
]

{ #category : 'executing' }
RBAddVariableTransformation >> privateTransform [

	isClassVariable
		ifTrue: [ self definingClass instanceSide addClassVariable: variableName ]
		ifFalse: [ self definingClass addInstanceVariable: variableName ]
]
