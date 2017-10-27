accessing
upTo: delimiter

	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ (ch := self next) == nil or: [ ch = delimiter ] ] 
			whileFalse: [ stream nextPut: ch ] ]
