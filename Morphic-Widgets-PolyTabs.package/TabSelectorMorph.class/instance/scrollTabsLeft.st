operations
scrollTabsLeft
	"Scroll left through the tabs."
	
	self visibleTabRange: (self visibleTabRange first < 2
		ifTrue: [0 to: self tabs size]
		ifFalse: [0 to: self visibleTabRange first - 1]).
	self updateTabs