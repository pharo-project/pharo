Extension { #name : 'HashedCollection' }

{ #category : '*DeepCopier' }
HashedCollection >> veryDeepCopyWith: deepCopier [
	| copyOfSelf|
	copyOfSelf := super veryDeepCopyWith: deepCopier.
	"force Sets and Dictionaries to rehash"
	copyOfSelf rehash.
		^ copyOfSelf
]
