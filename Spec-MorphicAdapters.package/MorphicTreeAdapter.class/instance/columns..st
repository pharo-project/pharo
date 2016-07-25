spec protocol
columns: newColumns

	self widgetDo: [ :w || columns |
		columns := newColumns collect: [ :each | each buildWithSpec ]. 
		w columns: columns.
		w resizerChanged.
		w updateList ]