abstract
basicCreationTime: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 