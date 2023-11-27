"
Removes a variable from a class. 
Take care that as a transformation, it does not care if there are accesses to the removed variable.
Use the corresponding refactoring if you need such insurance. 

Usage:

```
| refact |
refact := (RBRemoveVariableTransformation 
	variable: 'isClassVariable'
	class: #RBVariableTransformation
	classVariable: false).
(StRefactoringPreviewPresenter
		 changes: refact generateChanges)
		refactoring: refact;
		openModal
```

Preconditions:
- the class exists.
- the class defines a variable with the given name. Pay attention, the variable must defined in the class and not in a superclass. 


"
Class {
	#name : 'RBRemoveVariableTransformation',
	#superclass : 'RBVariableTransformation',
	#instVars : [
		'applicabilityPreconditions'
	],
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

	^ ( RBIsNotAMetaclass class: class )
		& ( RBDirectlyDefinesSharedVariableCondition class: class sharedVariables: {variableName} )
	
]

{ #category : 'preconditions' }
RBRemoveVariableTransformation >> applicabilityPreconditionsForInstanceVariable [
	"Only remove an instance variable that is locally defined in a class! Check comment on RBRemoveInstanceVariableRefactoring for a justification."
	
	^ applicabilityPreconditions ifNil: [ 
			applicabilityPreconditions := RBDirectlyDefinesInstanceVariableCondition 
						classNamed: class name 
						inModel: self model 
						instanceVariables: {variableName}
		 ]

]

{ #category : 'executing' }
RBRemoveVariableTransformation >> privateTransform [ 

	isClassVariable
		ifTrue: [ self definingClass instanceSide removeClassVariable: variableName ]
		ifFalse: [ self definingClass removeInstanceVariable: variableName ]
]
