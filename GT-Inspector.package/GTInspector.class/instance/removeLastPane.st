actions
removeLastPane
	| lastPane |
	
	lastPane := self first panes last.
	self first removeLastPane.
	self first announce: (GLMPaneRemoved pane: lastPane fromBrowser: self first).
	self first panes last updateToolbar