initialization
initialize
	"initialize the state of the receiver"
	
	super initialize.
	self enableDragNDrop.
	self clipSubmorphs: true