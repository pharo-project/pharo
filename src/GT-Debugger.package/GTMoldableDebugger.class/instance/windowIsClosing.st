callbacks
windowIsClosing 
	self flag: 'bug here when an announcement is inspected?'.
	self announce: (GLMBrowserClosing new presentation: self)