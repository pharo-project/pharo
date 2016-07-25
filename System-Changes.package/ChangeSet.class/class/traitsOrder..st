filein/out
traitsOrder: aCollection 
	"Arrange the traits in the collection, first who don't depend on others."

	| unprocessed |
	unprocessed := aCollection asSet. 
	^ Array new: unprocessed size streamContents: [ :stream |
		unprocessed size timesRepeat: [ 
			| aTrait |
			aTrait := unprocessed detect: [ :each | self hasNoDependenciesFor: each in: unprocessed ].
			stream nextPut: aTrait.
			unprocessed remove: aTrait] ]