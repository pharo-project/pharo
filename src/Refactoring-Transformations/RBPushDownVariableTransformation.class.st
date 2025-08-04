"
Removes this variable from the given class, then adds this variable in all the subclasses of this class.

Usage:
```
| transformation |
transformation := (RBPushDownVariableTransformation 
		instanceVariable: 'foo1'
		class: #RBLintRuleTestData)
		generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
overriden from RBAddVariableTransformation and RBRemoveVariableAccessorTransformation
"
Class {
	#name : 'RBPushDownVariableTransformation',
	#superclass : 'RBCompositeVariableTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBPushDownVariableTransformation >> applicabilityPreconditionsForClassVariable [

	^ { RBCondition
		  definesClassVariable: variableName
		  in: self definingClass }
]

{ #category : 'preconditions' }
RBPushDownVariableTransformation >> applicabilityPreconditionsForInstanceVariable [

	^ { RBCondition
		  definesInstanceVariable: variableName
		  in: self definingClass }
]

{ #category : 'executing' }
RBPushDownVariableTransformation >> buildTransformations [

	^ OrderedCollection new
		add: (RBRemoveVariableTransformation
				model: self model
				variable: variableName
				class: className
				classVariable: isClassVariable);
		addAll: (self variableDefinitionsInHierarchy
			collect: [ :each |
				RBAddVariableTransformation
				model: self model
				variable: variableName
				class: each
				classVariable: isClassVariable ]);
		yourself
]

{ #category : 'preconditions' }
RBPushDownVariableTransformation >> preconditions [

	^ isClassVariable
		ifTrue: [ self applicabilityPreconditionsForClassVariable ]
		ifFalse: [ self applicabilityPreconditionsForInstanceVariable ]
]

{ #category : 'storing' }
RBPushDownVariableTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	className storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	isClassVariable storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'api' }
RBPushDownVariableTransformation >> variable: aVariableName class: aClassName classVariable: aBoolean [

	variableName := aVariableName.
	isClassVariable := aBoolean.
	className := aClassName
]

{ #category : 'private' }
RBPushDownVariableTransformation >> variableDefinitionsInHierarchy [
	"For class variable return only classes that reference the class variable 
	both from instance and class side.
	For instance variable return all subclasses."

	self flag: #pharoTodo. "why the behavior is different for class and instance variable?"
	^ isClassVariable
		ifTrue: [ self definingClass allSubclasses reject: [ :each |
			(each whichSelectorsReferToClassVariable: variableName) isEmpty
			and: [ (each classSide whichSelectorsReferToClassVariable: variableName) isEmpty ] ] ]
		ifFalse: [ self definingClass subclasses ]
]
