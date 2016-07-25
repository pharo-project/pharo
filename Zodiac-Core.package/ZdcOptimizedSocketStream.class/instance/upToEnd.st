stream in
upToEnd
	"Read bytes until the stream is atEnd and return them as a ByteArray. Overwritten, optimized"

	^ self collectionSpecies 
		streamContents: [ :writeStream |
			[	
				readBuffer isEmpty ifFalse: [
					writeStream 
						next: readBuffer availableForReading 
						putAll: readBuffer buffer 
						startingAt: readBuffer contentsStart.
					readBuffer advanceReadPointer: readBuffer availableForReading ].   
				self atEnd ] whileFalse: [ 
					readBuffer isEmpty 
						ifTrue: [ 
							[ self fillReadBuffer ] 
								on: ConnectionClosed 
								do: [ :exception | exception return ] ] ] ]