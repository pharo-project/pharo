operations
ensureDeleteAll
	"Delete this directory and all children of it, and does not raise an error if the file does not exist."
	
	self exists ifFalse: [ ^ self  ].
	self deleteAll
	