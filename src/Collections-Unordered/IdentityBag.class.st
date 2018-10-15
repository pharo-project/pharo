"
Like a Bag, except that items are compared with #== instead of #= .

See the comment of IdentitySet for more information.

"
Class {
	#name : #IdentityBag,
	#superclass : #Bag,
	#category : #'Collections-Unordered-Bags'
}

{ #category : #'instance creation' }
IdentityBag class >> contentsClass [
	^IdentityDictionary
]

{ #category : #converting }
IdentityBag >> asSet [
	^contents keys asIdentitySet
]
