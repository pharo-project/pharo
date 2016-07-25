tabs
selectPreviousTab
	"Select the previous tab, or the last if none selected."
	
	self selectedIndex: (self selectedIndex < 2
		ifTrue: [self tabs size]
		ifFalse: [self selectedIndex - 1])