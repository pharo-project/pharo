callbacks
actOnPaneRemoved: ann

	self first panes last presentations first updateToolbar.
	self notifyPaneRemoved ifTrue: [ 
		self debugger announce: ann ]