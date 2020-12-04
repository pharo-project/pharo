Extension { #name : #OrderedDictionary }

{ #category : #'*Spec2-Core' }
OrderedDictionary >> replaceKey: aKey with: otherKey [
	| value index |
	
	value := self at: aKey.
	index := orderedKeys indexOf: aKey.
	dictionary removeKey: aKey.
	dictionary at: otherKey put: value.
	orderedKeys at: index put: otherKey
	
]
