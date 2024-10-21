"
A composite transformation is a transformation composed of other transformation. 
For example move instance variable can be composed of add and remove. 

In addition, a composite transformation can have previous transformations. 
A previous transformation is a list of transformations resulting from for example configuration such as
pre transformations. 
For example do we push up instance variable when we push up a method that uses such instance variable.

"
Class {
	#name : 'RBCompositeTransformation',
	#superclass : 'ReTransformation',
	#instVars : [
		'previousTransformations',
		'transformations'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'api' }
RBCompositeTransformation class >> model: aRBModel with: aCollection [

	^ self new
		model: aRBModel;
		transformations: aCollection;
		yourself
]

{ #category : 'api' }
RBCompositeTransformation class >> with: aCollection [

	^ self new
		transformations: aCollection;
		yourself
]

{ #category : 'executing' }
RBCompositeTransformation >> buildTransformations [

	self subclassResponsibility
]

{ #category : 'accessing' }
RBCompositeTransformation >> previousTransformations [
	^ previousTransformations ifNil: [ previousTransformations := OrderedCollection new ]
]

{ #category : 'transforming' }
RBCompositeTransformation >> privateTransform [
	"in refactoring mode, the transformation can do its own precondition checking,
	 therefore not needing checking from its containing transformations"

	"previousTransformations can be a transformation resulting from a given configuration,
	for example do we push up instance variable when we push up a method that uses such instance variable"

	self previousTransformations , self transformations do: [
		:transformation |
		transformation
			copyOptionsFrom: self options;
			model: self model;
			generateChanges ]
]

{ #category : 'accessing' }
RBCompositeTransformation >> transformations [

	^ transformations ifNil: [ transformations := self buildTransformations ]
]

{ #category : 'accessing' }
RBCompositeTransformation >> transformations: anObject [
	transformations := anObject
]
