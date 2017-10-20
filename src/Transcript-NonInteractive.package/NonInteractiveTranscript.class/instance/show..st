streaming
show: anObject
	"Output anObject asString on the receiver and show the output"
	
	self critical: [ self print: anObject; endEntry ]