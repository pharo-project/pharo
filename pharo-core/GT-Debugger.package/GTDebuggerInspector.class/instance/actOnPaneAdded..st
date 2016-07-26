callbacks
actOnPaneAdded: ann

	self panes size > 1 ifTrue: [ 
		(self panes before: self panes last) presentations first updateToolbar ]