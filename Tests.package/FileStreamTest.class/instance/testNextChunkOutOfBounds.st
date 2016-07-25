testing
testNextChunkOutOfBounds
	"self debug: #testNextChunkOutOfBounds"
	
	"Ensure that nextChunkPut:/nextChunk works properly on a caching file"
	| file text read filename |
	filename := 'testNextChunkOutOfBounds'.
	[file := FileStream forceNewFileNamed: filename.
	text := 'this is a chunkful of text'.
	file nextChunkPut: text.
	read := [file position: 999999; nextChunk] valueWithin: 1 seconds onTimeout:[nil].
	self assert: read = ''.
	] ensure: [ 
		file close.
		(FileSystem disk workingDirectory / filename) ensureDelete ].