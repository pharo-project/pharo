initialization
initialize
	super initialize.

	self color: Color transparent.
	
	self theme pagerWidgetThemer previewPaneStyleFor: self.
	self borderWidth: self defaultBorderWidth.