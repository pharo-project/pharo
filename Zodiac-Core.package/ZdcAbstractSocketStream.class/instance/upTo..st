stream in
upTo: value 
	"Read bytes upto but not including value and return them as a ByteArray.
	If value is not found, return the entire contents of the stream.
	This is an inefficient abstract implementation reading bytes one by one."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]