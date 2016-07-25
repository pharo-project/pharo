accessing
upTo: anObject 
	"We use our own collectionSpecies."
	
	^ self collectionSpecies streamContents: [ :out | | element |
		[ self atEnd or: [ (element := self next) = anObject ] ]
			whileFalse: [ out nextPut: element ] ]