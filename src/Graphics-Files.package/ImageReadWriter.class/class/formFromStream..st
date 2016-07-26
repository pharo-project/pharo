image reading/writing
formFromStream: aBinaryStream 
	"Answer a ColorForm stored on the given stream.  closes the stream"
	| reader readerClass form |
	readerClass := self readerClassFromStream: aBinaryStream.
	reader := readerClass new on: aBinaryStream.
	form := reader nextImage.
	reader close.
	^ form