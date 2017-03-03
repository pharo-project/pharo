initialization
initialize 
	super initialize.
	showIndex := 0.
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