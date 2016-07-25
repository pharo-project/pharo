*CodeExport
fileOutOn: aFileStream initializing: aBool
	"File a description of the receiver on aFileStream."

	self crTrace: self name.
	super fileOutOn: aFileStream.
	self class nonTrivial
		ifTrue: [
			aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; cr; cr.
			self class
				fileOutOn: aFileStream
				initializing: aBool ]