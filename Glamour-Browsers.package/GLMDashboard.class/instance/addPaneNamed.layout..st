adding
addPaneNamed: aSymbol layout: aLayout
	self addPaneNamed: aSymbol.
	self panesLayouts at: aSymbol put: aLayout