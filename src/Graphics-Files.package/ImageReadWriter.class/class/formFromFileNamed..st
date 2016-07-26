image reading/writing
formFromFileNamed: fileName 
	"Answer a ColorForm stored on the file with the given name."
	| stream |
	stream := FileStream readOnlyFileNamed: fileName.
	^ self formFromStream: stream