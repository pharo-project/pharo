Class {
	#name : 'RBEquivalentMethodRootHolder',
	#superclass : 'Object',
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'as yet unclassified' }
RBEquivalentMethodRootHolder >> methodWithSuper [

	^ 11 raisedTo: super someNumber factorial
]

{ #category : 'for tests' }
RBEquivalentMethodRootHolder >> simpleMethodReturn [

	^ 42
]

{ #category : 'as yet unclassified' }
RBEquivalentMethodRootHolder >> someNumber [

	^ 2
]
