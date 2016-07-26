*CodeExport
fileOutCommentOn: aFileStream
	"Copy the class comment to aFileStream."

	self classComment ifNil: [ ^ self ].
	aFileStream
		cr;
		nextChunkPut: self classComment string