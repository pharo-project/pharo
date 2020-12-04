"
An `IdentityDictionary` version that uses `==` instead of `=` for key comparing.
For more information about my usage, check `OrderedDictionary` and `Dictionary` comments.

###Examples

```
	object := Object new.
	otherObject := Object new.
	dict := OrderedIdentityDictionary new.
	dict
		at: object put: 1;
		at: otherObject put: 2.
	dict.		""returns: an OrderedIdentityDictionary(an Object->1 an Object->2)""
	dict at: object put: 3.
	dict 		""returns: an OrderedIdentityDictionary(an Object->3 an Object->2)""
```
"
Class {
	#name : #OrderedIdentityDictionary,
	#superclass : #OrderedDictionary,
	#category : #'Collections-Sequenceable-Ordered'
}

{ #category : #accessing }
OrderedIdentityDictionary >> dictionaryClass [
	^ IdentityDictionary
]

{ #category : #accessing }
OrderedIdentityDictionary >> indexOfKey: aKey ifAbsent: aBlock [
	^ self identityIndexOfKey: aKey ifAbsent: aBlock
]

{ #category : #private }
OrderedIdentityDictionary >> orderedKeysIndexOf: aKey [
	^ orderedKeys identityIndexOf: aKey
]
