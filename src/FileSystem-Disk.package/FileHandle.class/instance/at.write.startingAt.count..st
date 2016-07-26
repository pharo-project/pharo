public
at: index write: buffer startingAt: start count: count
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	Primitives 
		setPosition: id to: index - 1;
		write: id from: buffer startingAt: start count: count
