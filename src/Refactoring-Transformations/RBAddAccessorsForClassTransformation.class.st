"
Adds accessors (getters and setters) for all the variables in a class, if each of them do not exist.

Usage:
```
| transformation |
transformation := (RBAddAccessorsForClassTransformation
	className: #RBVariableTransformation)
	generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```
Preconditions:
overridden from RBAddVariableAccessorRefactoring
"
Class {
	#name : 'RBAddAccessorsForClassTransformation',
	#superclass : 'RBCompositeTransformation',
	#instVars : [
		'className'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBAddAccessorsForClassTransformation class >> className: aClassName [

	^ self new
		className: aClassName;
		yourself
]

{ #category : 'api' }
RBAddAccessorsForClassTransformation class >> model: aRBModel className: aClassName [

	^ self new
		model: aRBModel;
		className: aClassName;
		yourself
]

{ #category : 'executing' }
RBAddAccessorsForClassTransformation >> buildTransformations [

	| class |
	class := self model classObjectFor: className.
	^ (class instanceVariableNames collect: [ :variable |
			RBAddVariableAccessorTransformation
				model: self model
				instanceVariable: variable
				class: class ])
]

{ #category : 'api' }
RBAddAccessorsForClassTransformation >> className: aClassName [

	className := aClassName
]

{ #category : 'storing' }
RBAddAccessorsForClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' className: '.
	className storeOn: aStream.
	aStream nextPut: $)
]
