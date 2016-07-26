tests
testPanes
	| layout |
	layout := GLMDashboard new.
	layout addPaneNamed: #one.
	layout addPaneNamed: #two.
	layout addPaneNamed: #three.
	self assert: layout panes size equals: 3.
	#(#one #two #three) do: [ :aSymbol | self assert: (layout panes anySatisfy: [ :aGLMPane | aGLMPane name = aSymbol ]) ]