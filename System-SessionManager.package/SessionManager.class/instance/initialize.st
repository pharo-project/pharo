initialization
initialize
	super initialize.
	categories := OrderedCollection new.
		
	systemCategory := self createCategory: 'System'.
	networkCategory := self createCategory: 'Network'.
	guiCategory := self createCategory: 'Graphical User Interface'.
	toolsCategory := self createCategory: 'Tools'.
	userCategory := self createCategory: 'User'.