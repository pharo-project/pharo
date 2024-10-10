"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassBetaSibling',
	#superclass : 'MyClassAlpha',
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassBetaSibling >> methodDuplicatedInSibling [
	^ 38 * 101
]

{ #category : 'dummy methods' }
MyClassBetaSibling >> methodForSupersendInLeaf [
	^ 22
]

{ #category : 'dummy methods' }
MyClassBetaSibling >> sendSuper [
	^ super methodForSupersend 
]
