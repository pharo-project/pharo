tests
testPanesHaveDifferentRegistries
	| browser |
	browser := GLMFinder new.
	browser addPresentation: (GLMPresentation new title: 'presentation1').
	self assert: browser panes isEmpty.	"Outer entity changed."
	(browser pane port: #entity) value: #value1.
	self assert: browser panes size equals: 1.
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes size equals: 2.
	self deny: (browser panes first instVarNamed: 'registry') == (browser panes second instVarNamed: 'registry')