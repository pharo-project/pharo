initialization widgets
initializeStackPane
	
	self stackPane
		displayBlock: [ :aContext | self columnsFor: aContext ];
		items: (self debugger session stackOfSize: 50 ) ;
		whenSelectedItemChanged: [ :aContext | 
			"Set the selection before, as debugAction removes the link with the debugger. "
			self debugger stackPresentation selection: aContext.
			self openFullDebugger ]
		