public
flush
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	Primitives flush: id