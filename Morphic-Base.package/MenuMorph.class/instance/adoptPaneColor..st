actions
adoptPaneColor: paneColor
	"Change our color."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self color: paneColor