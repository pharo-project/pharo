"
Removes a variable from a class. It does not remove it if there is a direct access to this variable in the class.

Usage:

```
transformation := (RBRemoveVariableTransformation 
	variable: 'isClassVariable'
	class: #RBVariableTransformation
	classVariable: false)
	transform. 
(ChangesBrowser changes: transformation model changes changes) open
```

Preconditions:
- the class exists
- the class defines a variable with the given name
"
Class {
	#name : #RBRemoveVariableTransformation,
	#superclass : #RBVariableTransformation,
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #preconditions }
RBRemoveVariableTransformation >> preconditions [

	class := self definingClass instanceSide.

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
	"For now only remove an instance variable that is locally defined in a class."
	
	^ (RBCondition directlyDefinesInstanceVariable: variableName asString in: class)
		& (RBCondition doesNotAccessInstanceVariable: variableName inHierarchyOf: class showIn: self)
]

{ #category : #executing }
RBRemoveVariableTransformation >> privateTransform [

	isClassVariable
		ifTrue: [ self definingClass instanceSide removeClassVariable: variableName ]
		ifFalse: [ self definingClass removeInstanceVariable: variableName ]
]
