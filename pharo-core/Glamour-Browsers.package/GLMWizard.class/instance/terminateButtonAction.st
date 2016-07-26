accessing - buttons actions 
terminateButtonAction

	self externalTerminateButtonAction value: self entity.
	self announce: (GLMBrowserClosed new)
		