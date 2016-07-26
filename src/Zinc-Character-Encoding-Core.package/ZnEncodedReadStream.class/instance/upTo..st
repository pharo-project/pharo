accessing
upTo: anObject 	
	^ self collectionSpecies 
		streamContents: [ :out | | element |
			[ self atEnd or: [ (element := self next) = anObject ] ] whileFalse: [ 
				out nextPut: element ] ]