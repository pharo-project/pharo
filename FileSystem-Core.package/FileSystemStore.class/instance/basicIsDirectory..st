abstract
basicIsDirectory: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility 