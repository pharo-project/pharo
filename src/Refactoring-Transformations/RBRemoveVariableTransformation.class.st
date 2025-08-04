"
Removes a variable from a class. It does not remove it if there is a direct access to this variable in the class.

Usage:

```
| transformation |
transformation := (RBRemoveVariableTransformation 
	variable: 'isClassVariable'
	class: #RBVariableTransformation
	classVariable: false)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the class exists
- the class defines a variable with the given name
"
Class {
	#name : 'RBRemoveVariableTransformation',
	#superclass : 'RBVariableTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBRemoveVariableTransformation >> applicabilityPreconditions [

	class := self definingClass.

	^ isClassVariable
		  ifTrue: [ self applicabilityPreconditionsForClassVariable ]
		  ifFalse: [ self applicabilityPreconditionsForInstanceVariable ]
]

{ #category : 'preconditions' }
RBRemoveVariableTransformation >> applicabilityPreconditionsForClassVariable [

	^ { (RBCondition isMetaclass: class) not.
	    (RBCondition definesClassVariable: variableName in: class) }
]

{ #category : 'preconditions' }
RBRemoveVariableTransformation >> applicabilityPreconditionsForInstanceVariable [
	"For now only remove an instance variable that is locally defined in a class."
	
	^ { (ReDirectlyDefinesInstanceVariableCondition class: class inModel: self model instanceVariables: { variableName }) }
]

{ #category : 'executing' }
RBRemoveVariableTransformation >> privateTransform [

	isClassVariable
		ifTrue: [ self definingClass instanceSide removeClassVariable: variableName ]
		ifFalse: [ self definingClass removeInstanceVariable: variableName ]
]
