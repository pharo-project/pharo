testing
closed
	"Answer true if this file is closed."

	^ fileID == nil or: [ (self primSizeNoError: fileID) == nil ]
