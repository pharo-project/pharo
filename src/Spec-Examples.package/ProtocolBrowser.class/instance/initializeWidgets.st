initialization
initializeWidgets

	text := self newCode.
	viewer := self instantiate: ProtocolViewer.
	
	text aboutToStyle: true.

	self focusOrder 
		add: viewer;
		add: text