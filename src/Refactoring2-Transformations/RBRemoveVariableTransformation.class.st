"
Removes a variable from a class. It does not remove direct accesses to this variable in the class.

Usage:
transformation := (RBRemoveVariableTransformation 
	variable: 'isClassVariable'
	class: #RBVariableTransformation
	classVariable: false)
	transform. 
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class exists
- the class defines a variable with the given name
"
Class {
	#name : #RBRemoveVariableTransformation,
	#superclass : #RBVariableTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #preconditions }
RBRemoveVariableTransformation >> preconditions [

	class := self definingClass theNonMetaClass.

	^ isClassVariable
		ifTrue: [ self preconditionsForClassVariable ]
		ifFalse: [ self preconditionsForInstanceVariable ]
]

{ #category : #preconditions }
RBRemoveVariableTransformation >> preconditionsForClassVariable [
	
	^ (RBCondition isMetaclass: class) not
		& (RBCondition definesClassVariable: variableName in: class)
		& (RBCondition accessesClassVariable: variableName in: class showIn: self)
]

{ #category : #preconditions }
RBRemoveVariableTransformation >> preconditionsForInstanceVariable [

	^ (RBCondition definesInstanceVariable: variableName asString in: class) 
		& (RBCondition doesNotAccessInstanceVariable: variableName inHierarchyOf: class showIn: self)
]

{ #category : #executing }
RBRemoveVariableTransformation >> privateTransform [

	isClassVariable
		ifTrue: [ self definingClass theNonMetaClass removeClassVariable: variableName ]
		ifFalse: [ self definingClass removeInstanceVariable: variableName ]
]
