"
I am just like an OrderedCollection except that I hold my objects weakly.

In use, be aware that objects only referenced by me are not ""removed"" (like in OrderedCollection), but merely nilled. So guard against nils when iterating/accessing, and don't expect #size to represent the number of objects I hold. 

For more details, read my tests in WeakOrderedCollectionTest. 
"
Class {
	#name : #WeakOrderedCollection,
	#superclass : #OrderedCollection,
	#category : #'Collections-Weak-Base'
}

{ #category : #private }
WeakOrderedCollection class >> arrayType [
	^ WeakArray
]
