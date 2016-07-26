accessing
adoptPaneColor: paneColor
	"Change our border color too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self borderStyle baseColor: paneColor