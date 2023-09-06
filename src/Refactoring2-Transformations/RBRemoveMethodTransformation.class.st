"
Removes a method from a class. Only the selector of the method is needed.

Usage:
| transformation |
transformation := (RBRemoveMethodTransformation 
		selector: #transform
		from: RBRemoveMethodTransformation)
		transform. 
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
Checks whether there are any references to this method
"
Class {
	#name : #RBRemoveMethodTransformation,
	#superclass : #RBMethodTransformation,
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #api }
RBRemoveMethodTransformation class >> model: aRBModel selector: aSelector from: aClassName [

	^ self new
		model: aRBModel;
		selector: aSelector
		from: aClassName;
		yourself
]

{ #category : #api }
RBRemoveMethodTransformation class >> model: aRBModel selectors: aCollection from: aClassName [

	^ RBCompositeTransformation new
		transformations: (aCollection collect: [:e |
			self model: aRBModel
				  selector: e
				  from: aClassName] )
]

{ #category : #api }
RBRemoveMethodTransformation class >> selector: aSelector from: aClassName [

	^ self new
		selector: aSelector
		from: aClassName;
		yourself
]

{ #category : #api }
RBRemoveMethodTransformation class >> selectors: aCollection from: aClassName [

	^ self model: RBTransformation new model selectors: aCollection from: aClassName
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> applicabilityPreconditions [

	^ RBCondition definesSelector: selector in: self definingClass
]

{ #category : #executing }
RBRemoveMethodTransformation >> privateTransform [

	self definingClass removeMethod: selector
]

{ #category : #accessing }
RBRemoveMethodTransformation >> selector [

	^ selector 
]

{ #category : #api }
RBRemoveMethodTransformation >> selector: aSelector from: aClassName [

	self className: aClassName.
	selector := aSelector
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> senders [

	^ self sendersOf: { selector }
]

{ #category : #preconditions }
RBRemoveMethodTransformation >> sendersOf: aCollection [
	"This method expects a collection of selectors to have the same API than the corresponding refactoring. This should be revisited in the future."
	| occurrences |
	occurrences := OrderedCollection new.
	aCollection do: [ :each |
		self model allReferencesTo: each do: [ :aRBMethod |
			selector = aRBMethod selector ifFalse: [
				occurrences add: each -> aRBMethod ] ] ].
	^ occurrences
]
