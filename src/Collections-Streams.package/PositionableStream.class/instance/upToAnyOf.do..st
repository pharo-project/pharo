accessing
upToAnyOf: subcollection do: aBlock
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of any object in the collection.
	Evaluate aBlock with this occurence as argument.
	If no matching object is found, don't evaluate aBlock and answer the entire rest of the receiver."
	
	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ self atEnd or: [ (subcollection includes: (ch := self next)) and: [aBlock value: ch. true] ] ] 
			whileFalse: [ stream nextPut: ch ] ]