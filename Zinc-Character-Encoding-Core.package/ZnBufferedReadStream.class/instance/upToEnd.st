accessing
upToEnd
	"Read elements until the stream is atEnd and return them as a collection."

	^ self collectionSpecies
		streamContents: [ :out | 
			[ self atEnd ] whileFalse: [ 
				position > limit
					ifTrue: [ self nextBuffer ].	
				out next: limit - position + 1 putAll: buffer startingAt: position.
				position := limit + 1 ] ]