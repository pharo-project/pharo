accessing
formForImageEntity: imageEntity
	"Parse the actual bytes of imageEntity and return a Form."
	
	| imageType parserClassName parserClass parser |
	imageType := imageEntity contentType sub.
	parserClassName := imageType asUppercase, #ReadWriter.
	parserClass := Smalltalk globals at: parserClassName asSymbol.
	parser := parserClass on: imageEntity readStream.
	^ parser nextImage
	