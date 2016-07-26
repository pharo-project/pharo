operations
scrollTabsRight
	"Scroll right through the tabs."
	
	self visibleTabRange: (self visibleTabRange last \\ self tabs size + 1 to: 0).
	self updateTabs