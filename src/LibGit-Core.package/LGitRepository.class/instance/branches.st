accessing
branches
	^ OrderedCollection new: 10 streamContents: [ :stream |
		self branchesDo: [ :reference | stream nextPut: reference ] ]