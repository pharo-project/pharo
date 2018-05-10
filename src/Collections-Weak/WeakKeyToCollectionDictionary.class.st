"
This class represents an identity dictionary with weak keys, whose values are collections. 
Keys not in the dictionary are mapped to the empty collection.  Conversely, if a collection becomes empty, the mapping can be removed to save time and space.  However, because this requires re-hashing, it does not pay to do this to eagerly.
"
Class {
	#name : #WeakKeyToCollectionDictionary,
	#superclass : #WeakIdentityKeyDictionary,
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #finalization }
WeakKeyToCollectionDictionary >> finalizeValues [ 
	self rehash
]

{ #category : #private }
WeakKeyToCollectionDictionary >> noCheckAddForRehash: anAssociation [
	| cleanedValue |
	anAssociation key ifNil:[^self].
	cleanedValue := anAssociation value copyWithout: nil.
	cleanedValue notEmpty ifTrue:[
		anAssociation value: cleanedValue.
		super noCheckAddForRehash: anAssociation.
	].
]

{ #category : #private }
WeakKeyToCollectionDictionary >> noCheckNoGrowFillFrom: anArray [
	"Add the elements of anArray except nils and associations with empty collections (or with only nils) to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	tally := 0.
	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :association |
			association key ifNotNil: [ :key | "Don't let the key go away"
				| cleanedValue |
				(cleanedValue := association value copyWithout: nil) isEmpty 
					ifFalse: [
						association value: cleanedValue.
						array
							at: (self scanForEmptySlotFor: key)
							put: association.
						tally := tally + 1 ] ] ] ]
]
