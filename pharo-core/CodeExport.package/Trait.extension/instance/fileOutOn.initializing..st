*CodeExport
fileOutOn: aFileStream initializing: aBool
	"File a description of the receiver on aFileStream"

	self crTrace: name.
	super fileOutOn: aFileStream.
	self hasClassSide ifTrue: [
		aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; cr; cr.
		self classTrait fileOutOn: aFileStream ]