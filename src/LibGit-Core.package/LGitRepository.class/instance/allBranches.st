accessing
allBranches
	^ OrderedCollection new: 10 streamContents: [ :stream |
		self allBranchesDo: [ :reference | 
			stream nextPut: reference ] ]