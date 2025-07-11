Class {
	#name : #RBCompositeMethodTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		#class,
		#selector
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #accessing }
RBCompositeMethodTransformation >> definingClass [

	^ self model classObjectFor: class
]

{ #category : #accessing }
RBCompositeMethodTransformation >> definingMethod [

	^ self definingClass parseTreeForSelector: selector asSymbol
]
