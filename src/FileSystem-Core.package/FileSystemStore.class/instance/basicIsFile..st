abstract
basicIsFile: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self subclassResponsibility 