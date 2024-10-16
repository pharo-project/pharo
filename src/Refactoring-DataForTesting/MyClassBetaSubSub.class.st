Class {
	#name : 'MyClassBetaSubSub',
	#superclass : 'MyClassBetaSub',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'as yet unclassified' }
MyClassBetaSubSub >> overridenInSubclassAndNoWhereElse [

	^ super overridenInSubclassAndNoWhereElse + 1
]
