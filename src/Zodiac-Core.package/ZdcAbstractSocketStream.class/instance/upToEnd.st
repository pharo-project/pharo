stream in
upToEnd
	"Read bytes until the stream is atEnd and return them as a ByteArray.
	This is an inefficient abstract implementation reading bytes one by one.
	Note that even when #atEnd returns false, the following #next could be nil 
	or the connection could suddenly be closed"

	^ self collectionSpecies 
		streamContents: [ :writeStream |
			[ [ self atEnd or: [ self peek isNil ] ] whileFalse: [ 
				writeStream nextPut: self next ] ]
		 		on: ConnectionClosed
				do: [ :exception | exception return ] ]