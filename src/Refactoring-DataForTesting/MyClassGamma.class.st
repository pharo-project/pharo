"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassGamma',
	#superclass : 'MyClassBeta',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassGamma >> referenceToClassAlpha [
	^ MyClassAlpha new
]
