callbacks
actOnPaneAdded: ann

	(self panes atLast: 2 ifAbsent: [ ^ self ])
		presentations first updateToolbar