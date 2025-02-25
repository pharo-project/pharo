Class {
	#name : 'RBEquivalentMethodRootHolder',
	#superclass : 'Object',
	#category : 'Refactoring-DataForTesting-MiniHierarchy',
	#package : 'Refactoring-DataForTesting',
	#tag : 'MiniHierarchy'
}

{ #category : 'for tests' }
RBEquivalentMethodRootHolder >> methodWithSuper [

	^ 11 raisedTo: super someNumber factorial
]

{ #category : 'for tests' }
RBEquivalentMethodRootHolder >> simpleMethodReturn [

	^ 42
]

{ #category : 'for tests' }
RBEquivalentMethodRootHolder >> someNumber [

	^ 2
]
