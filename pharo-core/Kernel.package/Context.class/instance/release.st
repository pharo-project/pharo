debugger access
release
	"Remove information from the receiver and all of the contexts on its 
	sender chain in order to break circularities."

	self releaseTo: nil