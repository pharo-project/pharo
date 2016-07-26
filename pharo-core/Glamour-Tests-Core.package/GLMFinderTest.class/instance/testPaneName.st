tests
testPaneName
	| browser |
	browser := GLMFinder new.
	browser addPresentation: (GLMPresentation new title: 'presentation1').
	(browser pane port: #entity) value: #value1.
	self assert: browser panes first name equals: '1'.
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes last name equals: '2'