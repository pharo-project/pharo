actions
adoptPaneColor: paneColor
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self selectionColor: self selectionColor.
	self borderStyle baseColor: paneColor twiceDarker