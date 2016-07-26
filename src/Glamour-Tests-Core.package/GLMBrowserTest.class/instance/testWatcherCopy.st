tests
testWatcherCopy
	| browser pane transmission browserCopy |
	browser := GLMBrowser new.
	pane := GLMPane named: 'one'.
	browser addPane: pane.
	transmission := GLMTransmission new.
	browser addTransmission: transmission.
	transmission addActiveOrigin: (browser panes first port: #origin).
	transmission toWatcher.
	browserCopy := browser copy.
	self assert: browserCopy transmissions size equals: 1.
	self assert: (browserCopy panes first port: #origin) value equals: nil.
	self assert: (browserCopy watcherPane port: #entity) value equals: nil.
	(browserCopy panes first port: #origin) value: 42.
	browserCopy transmissions first transmit.
	self assert: (browserCopy watcherPane port: #entity) value equals: 42