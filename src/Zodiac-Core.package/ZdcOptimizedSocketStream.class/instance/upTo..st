stream in
upTo: value 
	"Read bytes upto but not including value and return them as a ByteArray.
	If value is not found, return the entire contents of the stream. Overwritten, optimized"
	
	^ self collectionSpecies 
		streamContents: [ :writeStream |
			[ | position |
				position := 0.
				readBuffer isEmpty ifFalse: [ | count |
					position := readBuffer buffer indexOf: value startingAt: readBuffer contentsStart.
					count := position = 0 ifTrue: [ readBuffer availableForReading ] ifFalse: [ position - readBuffer contentsStart ].
					writeStream next: count putAll: readBuffer buffer startingAt: readBuffer contentsStart.
					readBuffer advanceReadPointer: count.
					position = 0 ifFalse: [ readBuffer advanceReadPointer: 1 ] ].   
				position ~= 0 or: [ self atEnd ] ] whileFalse: [ 
					readBuffer isEmpty ifTrue: [ self fillReadBuffer ] ] ]