private
removePaneIndex: i
	| paneRemoved |
	paneRemoved := self panes removeAt: i.
	paneRemoved unregisterFromAllAnnouncements.
	self announce: (GLMPaneRemoved pane: paneRemoved fromBrowser: self )