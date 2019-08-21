"
A WeakClassVariable is a class variable that references an object weakly.

Internally it is implemented with a weak array of size one. Writing into the variable writes the only field in the array. 

weakArray at: 1 put: aValue.

Reading into the variable reads the only field in the array.

weakArray at: 1
"
Class {
	#name : #WeakClassVariable,
	#superclass : #LiteralVariable,
	#category : #'Slot-Examples-ClassVariables'
}

{ #category : #initialization }
WeakClassVariable >> installingIn: aClass [
	"when the var gets installed, initialize it"
	super write: (WeakArray new: 1)
]

{ #category : #'meta-object-protocol' }
WeakClassVariable >> read [

	^ super read at: 1
]

{ #category : #'meta-object-protocol' }
WeakClassVariable >> write: aValue [

	^super read at: 1 put: aValue
]
