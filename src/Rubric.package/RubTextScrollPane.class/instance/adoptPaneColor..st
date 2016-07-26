accessing
adoptPaneColor: paneColor
	"Adopt the given pane color."
	
	super adoptPaneColor: paneColor.
	self hScrollbar adoptPaneColor: paneColor darker.
	self vScrollbar adoptPaneColor: paneColor darker