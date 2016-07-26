callbacks
actOnBrowserClosing: ann
	self session ifNotNil: [ :aSession | aSession terminate ]