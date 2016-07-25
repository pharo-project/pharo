initialization
initialize 
	super initialize.
	showColumnHeaders := true.
	selectedRowIndexes := #().
	highlightedRowIndexes := #().
	columns := #().
	needToggleAtMouseUp := false.
	self beNotResizable.
	self beSingleSelection.
	self enableSearch.
	self initializeScrollBars.
	self initializeContainer.
	self initializeKeyBindings.
	
	self resizeAllSubviews