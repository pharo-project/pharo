tests
testPublishingToOutsideFinder
	| innerBrowser finder |
	finder := GLMFinder new.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #any.
	innerBrowser
		transmit to: #any;
		andShow: [ :a | a list display: [ :x | 1 to: 43 ] ].
	innerBrowser transmit toOutsidePort: #selection; from: #any port: #selection.
	finder show: [:a | a custom: innerBrowser].
	finder startOn: 43.
	self assert: (finder panes first port: #entity) value equals: 43.
	self assert: (finder panes first port: #selection) value isNil.
	((finder panes first presentations first paneNamed: #any) port: #selection) value: 42.
	self assert: (finder panes first port: #selection) value equals: 42.
	self assert: (finder panes first port: #entity) value equals: 43.
	self assert: finder panes size equals: 2.
	self assert: (finder panes last port: #entity) value equals: 42.
	self assert: (finder panes first presentations first isKindOf: GLMTabulator).
	self assert: (finder panes last presentations first isKindOf: GLMTabulator).
	self assert: (finder panes first presentations first isKindOf: GLMTabulator)