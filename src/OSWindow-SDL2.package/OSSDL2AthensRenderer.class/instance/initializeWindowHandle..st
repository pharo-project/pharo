initialization
initializeWindowHandle: aWindowHandle 
	self initialize.
	self window: aWindowHandle.
	renderer := window handle createDefaultRenderer.
	self resetResources.
	
		