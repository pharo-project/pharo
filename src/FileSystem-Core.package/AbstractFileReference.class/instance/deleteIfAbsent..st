operations
deleteIfAbsent: aBlock
	"Delete the receiver, when it does not exist evaluate the block"
	self resolve deleteIfAbsent: aBlock