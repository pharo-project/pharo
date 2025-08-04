"
I am responsible for moving a method from one class to another existing class

Usage:
```
| transformation |
transformation := (RBMoveMethodToClassTransformation
				method: RBMoveMethodToClassTransformation>>#storeOn:
				class: #RBMoveMethodParametrizedTest)
				generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

"
Class {
	#name : 'RBMoveMethodToClassTransformation',
	#superclass : 'RBMethodTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'initialization' }
RBMoveMethodToClassTransformation class >> method: aMethod class: aClass [
	^ self new
		method: aMethod class: aClass;
		yourself
]

{ #category : 'initialization' }
RBMoveMethodToClassTransformation class >> model: aRBSmalltalk method: aMethod class: aClass [
	^ self new
		model: aRBSmalltalk;
		method: aMethod class: aClass;
		yourself
]

{ #category : 'preconditions' }
RBMoveMethodToClassTransformation >> applicabilityPreconditions [

	^ { (RBCondition definesSelector: method selector in: class) not }
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> classModelOf: aClass [
	^ self model classObjectFor: aClass
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> method: aMethod class: aClass [
	method := aMethod.
	class := self model classObjectFor: aClass
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> privateTransform [
	| oldClass newClass rbMethod originalProtocol |
	oldClass := self classModelOf: method methodClass.
	newClass := self classModelOf: class.
	rbMethod := model rbMethodClass for: newClass source: method sourceCode selector: method selector.
	originalProtocol := method protocolName.

	self generateChangesFor: (RBRemoveMethodTransformation 
		selector: method selector
		from: oldClass).
	self generateChangesFor: (RBAddMethodTransformation
		model: self model
		sourceCode: rbMethod
		in: newClass
		withProtocol: originalProtocol)
]

{ #category : 'storing' }
RBMoveMethodToClassTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll:  method selector;
		nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ''')'
]
