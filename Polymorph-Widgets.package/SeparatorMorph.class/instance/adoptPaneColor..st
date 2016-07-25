accessing
adoptPaneColor: paneColor
	"Change our fill too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: (self theme separatorFillStyleFor: self)