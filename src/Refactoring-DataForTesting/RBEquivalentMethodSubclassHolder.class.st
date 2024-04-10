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
RBEquivalentMethodSubclassHolder >> simpleLocalMethodReturn [

	^ 42 + 33
]
