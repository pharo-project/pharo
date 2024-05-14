Class {
	#name : 'MyClassGamma',
	#superclass : 'MyClassBeta',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'as yet unclassified' }
MyClassGamma >> referenceToClassAlpha [
	^ MyClassAlpha new
]
