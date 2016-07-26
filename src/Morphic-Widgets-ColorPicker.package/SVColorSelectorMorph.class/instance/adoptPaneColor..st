accessing
adoptPaneColor: paneColor
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	self borderStyle baseColor: paneColor twiceDarker