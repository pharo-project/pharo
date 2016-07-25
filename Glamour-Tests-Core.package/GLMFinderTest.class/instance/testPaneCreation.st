tests
testPaneCreation
	| browser |
	browser := GLMFinder new.
	browser addPresentation: (GLMPresentation new title: 'presentation1').
	self assert: browser panes isEmpty.	"Outer entity changed."
	(browser pane port: #entity) value: #value1.
	self assert: browser panes size equals: 1.
	self assert: browser panes first browser == browser.
	self assert: (browser panes first port: #entity) value equals: #value1.
	self assert: browser panes first presentations size equals: 1.
	self assert: browser panes first presentations first title equals: 'presentation1'.
	self assert: browser panes first presentations first ~~ browser transmission transmissionStrategy presentations first.	"First pane selection changed."
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes size equals: 2.
	self assert: browser panes last browser == browser.
	self assert: (browser panes last port: #entity) value equals: #value2.
	self assert: browser panes last presentations first title equals: 'presentation1'.
	self assert: browser panes last presentations first ~~ browser transmission transmissionStrategy presentations first.
	self assert: browser panes last presentations first ~~ browser panes first presentations first.	"Second pane selection changed."
	(browser panes last port: #selection) value: #value3.
	self assert: browser panes size equals: 3.
	self assert: browser panes last browser == browser.
	self assert: (browser panes last port: #entity) value equals: #value3.	"First pane selection changed again."
	(browser panes first port: #selection) value: #value4.
	self assert: browser panes size equals: 2.
	self assert: browser panes last browser == browser.
	self assert: (browser panes last port: #entity) value equals: #value4.	"Outer entity changed again."
	(browser pane port: #entity) value: #value5.
	self assert: browser panes size equals: 1.
	self assert: browser panes last browser == browser.
	self assert: (browser panes last port: #entity) value equals: #value5