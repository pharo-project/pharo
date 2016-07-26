tests
testScriptingDefaults
	| browser |
	browser := GLMTabulator new.
	browser startOn: 42.
	self assert: browser panes size equals: 1.
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions first origins first equals: (browser pane port: #entity).
	self assert: browser transmissions first destination equals: (browser panes first port: #entity)