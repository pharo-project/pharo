accessing
scrollTarget: aTextArea
	self unregisterTextArea.
	super scrollTarget: aTextArea.
	self registerTextArea 