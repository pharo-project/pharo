"
Removes a method from a class. Only the selector of the method is needed.

Usage:
```
| transformation |
transformation := (RBRemoveMethodTransformation 
		selector: #privateTransform
		from: RBRemoveMethodTransformation)
		generateChanges. 
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
Checks whether there are any references to this method
"
Class {
	#name : 'RBRemoveMethodTransformation',
	#superclass : 'RBMethodTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveMethodTransformation class >> model: aRBModel selector: aSelector from: aClassName [

	^ self new
		model: aRBModel;
		selector: aSelector
		from: aClassName;
		yourself
]

{ #category : 'api' }
RBRemoveMethodTransformation class >> model: aRBModel selectors: aCollection from: aClassName [

	^ RBCompositeTransformation new
		transformations: (aCollection collect: [:e |
			self model: aRBModel
				  selector: e
				  from: aClassName] )
]

{ #category : 'api' }
RBRemoveMethodTransformation class >> selector: aSelector from: aClassName [

	^ self new
		selector: aSelector
		from: aClassName;
		yourself
]

{ #category : 'api' }
RBRemoveMethodTransformation class >> selectors: aCollection from: aClassName [

	^ self model: ReTransformation new model selectors: aCollection from: aClassName
]

{ #category : 'preconditions' }
RBRemoveMethodTransformation >> applicabilityPreconditions [

	^ { (RBCondition definesSelector: selector in: self definingClass) }
]

{ #category : 'executing' }
RBRemoveMethodTransformation >> privateTransform [

	self definingClass removeMethod: selector
]

{ #category : 'accessing' }
RBRemoveMethodTransformation >> selector [

	^ selector 
]

{ #category : 'api' }
RBRemoveMethodTransformation >> selector: aSelector from: aClassName [

	self className: aClassName.
	selector := aSelector
]
