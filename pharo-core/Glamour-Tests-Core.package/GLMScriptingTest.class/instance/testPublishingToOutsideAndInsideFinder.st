tests
testPublishingToOutsideAndInsideFinder
	| innerBrowser finder realInnerBrowser |
	finder := GLMFinder new.
	innerBrowser := GLMTabulator new.
	innerBrowser
		column: #one;
		column: #two.
	innerBrowser
		transmit to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: 43 ] ].
	innerBrowser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a text ].
	innerBrowser transmit toOutsidePort: #selection; from: #one port: #selection.
	finder show: [:a | a custom: innerBrowser].
	self assert: innerBrowser transmissions size equals: 3.
	self assert: innerBrowser transmissions second origins first pane == innerBrowser panes first.
	self assert: innerBrowser transmissions second destination pane == innerBrowser panes last.
	finder startOn: 43.
	realInnerBrowser := innerBrowser.
	self assert: realInnerBrowser transmissions size equals: 3.
	self assert: realInnerBrowser transmissions second origins first pane == realInnerBrowser panes first.
	self assert: realInnerBrowser transmissions second destination pane == realInnerBrowser panes last