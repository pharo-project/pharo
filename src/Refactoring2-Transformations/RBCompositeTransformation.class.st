Class {
	#name : #RBCompositeTransformation,
	#superclass : #RBTransformation,
	#instVars : [
		'transformations',
		'delegatesPreconditions'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBCompositeTransformation class >> model: aRBModel with: aCollection [

	^ (self new)
		model: aRBModel;
		transformations: aCollection;
		yourself
]

{ #category : #api }
RBCompositeTransformation class >> with: aCollection [

	^ (self new)
		transformations: aCollection;
		yourself
]

{ #category : #executing }
RBCompositeTransformation >> buildTransformations [

	self subclassResponsibility
]

{ #category : #accessing }
RBCompositeTransformation >> delegatesPreconditions: aBoolean [

	delegatesPreconditions := aBoolean
]

{ #category : #initialization }
RBCompositeTransformation >> initialize [

	super initialize.
	delegatesPreconditions := true
]

{ #category : #preconditions }
RBCompositeTransformation >> preconditions [
	"it delegates the preconditionn checking to each sub-refactoring separately.
	 additional preconditions are welcomed"

	^ RBCondition empty
]

{ #category : #executing }
RBCompositeTransformation >> privateTransform [
	"in refactoring mode, the transformation can do its own precondition checking,
	 therefore not needing checking from its containing transformations"

	self transformations do: [ :transformation |
		transformation
			copyOptionsFrom: self options;
			model: self model;
			preconditionChecking: (delegatesPreconditions & self preconditionChecking);
			transform ]
]

{ #category : #accessing }
RBCompositeTransformation >> transformations [

	^ transformations ifNil: [ transformations := self buildTransformations ]
]

{ #category : #accessing }
RBCompositeTransformation >> transformations: anObject [
	transformations := anObject
]
