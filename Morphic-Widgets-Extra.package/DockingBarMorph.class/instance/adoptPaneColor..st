accessing
adoptPaneColor: paneColor
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	originalColor :=  paneColor.
	self borderStyle baseColor: paneColor.
	self updateColor