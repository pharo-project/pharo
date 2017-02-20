actions
removeLastPane
	| lastPane |
	
	lastPane := self panes last.
	self browser removeLastPane.
	self browser announce: (GLMPaneRemoved pane: lastPane fromBrowser: self browser)