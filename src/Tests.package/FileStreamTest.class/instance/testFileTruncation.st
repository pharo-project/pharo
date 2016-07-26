testing
testFileTruncation
	"Ensure that nextChunkPut:/nextChunk works properly on a caching file"

	"self debug: #testFileTruncation"

	| file |
	file := 'TruncationTest.txt' asFileReference.
	[ 
	file writeStreamDo: [ :s | s nextPutAll: '1234567890' ].
	file
		writeStreamDo: [ :s | 
			self assert: s contents equals: '1234567890'.
			s truncate: 4 ].
	file readStreamDo: [ :s | self assert: s contents equals: '1234' ] ]
		ensure: [ file ensureDelete ]