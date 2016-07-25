accessing
upToEnd
	"We use our own collectionSpecies and read by chunk"

	^ self collectionSpecies streamContents: [ :out |
		[ self atEnd ] whileFalse: [ 
			out next: limit putAll: chunk.
			position := limit ] ]