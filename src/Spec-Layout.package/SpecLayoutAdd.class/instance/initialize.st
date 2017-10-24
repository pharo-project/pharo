initialization
initialize

	super initialize.
	self selector: #add:.
	layoutFrame := SpecLayoutFrame identity.
	subwidget := nil.