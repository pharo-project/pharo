*Polymorph-Widgets
noteNewOwner: o
	"Update the fill style."
	
	super noteNewOwner: o.
	self defer: [self adoptPaneColor: self paneColor]