Class {
	#name : 'RBEquivalentMethodSubclassHolder',
	#superclass : 'RBEquivalentMethodRootHolder',
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'for tests' }
RBEquivalentMethodSubclassHolder >> anotherMethod: arg [

	^ arg raisedTo: 5
]

{ #category : 'for tests' }
RBEquivalentMethodSubclassHolder >> methodToExtractFrom [

	| a |
	a := 7 raisedTo: super someNumber.
	^ a * 1
]

{ #category : 'for tests' }
RBEquivalentMethodSubclassHolder >> methodToExtractFromWithSuperAndSimilarMethodInSuper [

	| a |
	a := 11 raisedTo: super someNumber factorial.
	^ a * 1
]

{ #category : 'for tests' }
RBEquivalentMethodSubclassHolder >> methodWithSuperSend [

	^ 7 raisedTo: super someNumber
]

{ #category : 'for tests' }
RBEquivalentMethodSubclassHolder >> simpleLocalMethodReturn [

	^ 42 + 33
]

{ #category : 'for tests' }
RBEquivalentMethodSubclassHolder >> someNumber [

	^ 1
]
