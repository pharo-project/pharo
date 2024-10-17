Class {
	#name : 'MyClassBetaSubSub',
	#superclass : 'MyClassBetaSub',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassBetaSubSub >> overridenInSubclassAndNoWhereElse [

	^ super overridenInSubclassAndNoWhereElse + 1
]
