actions
adoptPaneColor: paneColor
	"Change the fill styles on the descriptions."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self descriptionMorph borderStyle: (self theme listNormalBorderStyleFor: self).
	self updateDescriptionFillStyle: paneColor
	