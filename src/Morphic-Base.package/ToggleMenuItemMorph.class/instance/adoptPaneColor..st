accessing
adoptPaneColor: paneColor
	"Pass on to submenu too."
	
	super adoptPaneColor: paneColor.
	self hasSubMenu ifTrue: [self subMenu adoptPaneColor: paneColor]