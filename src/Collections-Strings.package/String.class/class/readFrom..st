instance creation
readFrom: inStream
	"Answer an instance of me that is determined by reading the stream, 
	inStream. Embedded double quotes become the quote Character."

	| char done |
	^ self streamContents: [ :outStream | 
		"go to first quote"
		inStream skipTo: $'.
		done := false.
		[ done or: [ inStream atEnd ] ]
			whileFalse: [ 
				char := inStream next.
				char = $'
					ifTrue: [ 
						char := inStream next.
						char = $'
							ifTrue: [ outStream nextPut: char ]
							ifFalse: [ done := true ] ]
					ifFalse: [ outStream nextPut: char ] ] ]