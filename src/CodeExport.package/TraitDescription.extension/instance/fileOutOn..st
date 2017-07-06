*CodeExport
fileOutOn: aFileStream
	"File a description of the receiver on aFileStream."

	aFileStream nextChunkPut: self definition.
	self organization putCommentOnFile: aFileStream forClass: self.
	self organization categories 
		do: [ :heading | self fileOutLocalMethodsInCategory: heading on: aFileStream ]