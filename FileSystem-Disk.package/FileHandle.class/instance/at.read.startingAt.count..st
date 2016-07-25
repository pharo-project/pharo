public
at: index read: buffer startingAt: start count: count
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ Primitives 
		setPosition: id to: index - 1;
		read: id into: buffer startingAt: start count: count
