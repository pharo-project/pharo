streaming
crShow: anObject
	"Output anObject asString on the receiver preceded by a cr and show the output"
	
	self critical: [ self cr; show: anObject ]