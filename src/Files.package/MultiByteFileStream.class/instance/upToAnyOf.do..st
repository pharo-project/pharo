accessing
upToAnyOf: delimiters do: aBlock

	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ (ch := self next) == nil or: [ (delimiters includes: ch) and: [aBlock value: ch. true] ] ] 
			whileFalse: [ stream nextPut: ch ] ]