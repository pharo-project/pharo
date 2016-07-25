accessing
upToEnd
	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]