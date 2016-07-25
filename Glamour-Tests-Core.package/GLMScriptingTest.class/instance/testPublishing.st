tests
testPublishing
	| browser |
	browser := GLMTabulator new.
	browser column: #classes.
	browser sendToOutside: #selectedClass from: #classes -> #selection.
	browser sendTo: #classes -> #highlight fromOutside: #highlightedClass.
	(browser panes first port: #selection) value: 42.
	self assert: (browser pane port: #selectedClass) value equals: 42.
	self assert: (browser panes first port: #selection) value equals: 42.
	(browser pane port: #highlightedClass) value: 43.
	self assert: (browser pane port: #highlightedClass) value equals: 43.
	self assert: (browser panes first port: #highlight) value equals: 43.
	(browser panes first port: #highlight) value: 44.
	self assert: (browser pane port: #highlightedClass) value equals: 43.
	self assert: (browser panes first port: #highlight) value equals: 44.
	(browser pane port: #selectedClass) value: 45.
	self assert: (browser pane port: #selectedClass) value equals: 45.
	self assert: (browser panes first port: #selection) value equals: 42