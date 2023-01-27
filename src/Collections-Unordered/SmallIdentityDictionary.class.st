"
I'm a SmallDictionary (this means faster than default one when dealing with limited number of items)
but I check my key based on identity.
"
Class {
	#name : #SmallIdentityDictionary,
	#superclass : #SmallDictionary,
	#category : #'Collections-Unordered-Dictionaries'
}

{ #category : #private }
SmallIdentityDictionary >> findIndexForKey: aKey [
	^ keys identityIndexOf: aKey
]

{ #category : #private }
SmallIdentityDictionary >> setClass [
	^ IdentitySet
]
