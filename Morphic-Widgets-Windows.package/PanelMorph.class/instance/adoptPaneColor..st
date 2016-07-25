accessing
adoptPaneColor: paneColor
	"Change our color too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self assureExtension.
	extension fillStyle ifNil: [self color: paneColor].
	self borderStyle baseColor: paneColor darker