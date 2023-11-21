Extension { #name : 'OrderedDictionary' }

{ #category : '*Collections-Unordered' }
OrderedDictionary >> at: key update: updateBlock [
	"I am used to update the value at a given key, or if the key does not exist, to throw an error"
	self at: key update: updateBlock initial: [ KeyNotFound signalFor: key ]
]

{ #category : '*Collections-Unordered' }
OrderedDictionary >> dictionaryClass [
	^ Dictionary
]
