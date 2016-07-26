abstract
basicIsWritable: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self subclassResponsibility 