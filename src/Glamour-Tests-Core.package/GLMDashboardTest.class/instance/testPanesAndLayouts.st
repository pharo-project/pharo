tests
testPanesAndLayouts
	| layout |
	layout := GLMDashboard new.
	layout addPaneNamed: #one layout: (0 @ 0 corner: 0.5 @ 0.5) asLayoutFrame.
	layout addPaneNamed: #two layout: (0 @ 0.5 corner: 0.5 @ 1) asLayoutFrame.
	layout addPaneNamed: #three layout: (0.5 @ 0 corner: 1 @ 1) asLayoutFrame.
	layout addPaneNamed: #four.
	self assert: layout panes size equals: 4.
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #one) with: (0 @ 0 corner: 0.5 @ 0.5) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #two) with: (0 @ 0.5 corner: 0.5 @ 1) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #three) with: (0.5 @ 0 corner: 1 @ 1) asLayoutFrame)