initialization widgets
initializeStackPane
	
	self stackPane
		displayBlock: [ :aContext | self columnsFor: aContext ];
		items: (self debugger stack filteredStackOfSize: self debugger stack class defaultStackSize) ;
		whenSelectedItemChanged: [ :aContext | 
			"Set the selection before, as debugAction removes the link with the debugger. "
			self debugger stack setSelectedItem: aContext.
			self openFullDebugger ]
		