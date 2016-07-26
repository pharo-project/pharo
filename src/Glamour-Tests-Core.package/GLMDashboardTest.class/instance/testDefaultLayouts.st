tests
testDefaultLayouts
	| layout |
	layout := GLMDashboard new.
	layout addPaneNamed: #one.
	layout addPaneNamed: #two.
	layout addPaneNamed: #three.
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #one) with: (0 @ 0 corner: 0.5 @ 0.5) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #two) with: (0.5 @ 0 corner: 1 @ 0.5) asLayoutFrame).
	self assert: (self compareLayout: (layout layoutOfPaneNamed: #three) with: (0 @ 0.5 corner: 1 @ 1) asLayoutFrame)