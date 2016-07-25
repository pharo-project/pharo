initialization
initialize

	super initialize.
	self selector: #add:.
	layoutFrame := LayoutFrame identity.
	subwidget := nil.