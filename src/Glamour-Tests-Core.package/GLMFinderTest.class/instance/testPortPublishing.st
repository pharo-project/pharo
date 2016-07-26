tests
testPortPublishing
	| browser |
	browser := GLMFinder new.
	(browser pane port: #entity) value: #value1.
	self assert: browser panes size equals: 1.
	self assert: browser pane ports size equals: 1.
	self assert: (browser pane port: #entity) value equals: #value1.
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes size equals: 2.
	self assert: browser pane ports size equals: 3.
	self assert: (browser pane port: #entity) value equals: #value1.
	self assert: (browser pane port: #selection) value equals: #value2.
	(browser panes last port: #selection) value: #value3.
	self assert: browser panes size equals: 3.
	self assert: browser pane ports size equals: 3.
	self assert: (browser pane port: #entity) value equals: #value1.
	self assert: (browser pane port: #selection) value equals: #value3.
	(browser panes first port: #hover) value: #value4.
	self assert: browser panes size equals: 3.
	self assert: browser pane ports size equals: 4.
	self assert: (browser pane port: #entity) value equals: #value1.
	self assert: (browser pane port: #selection) value equals: #value3.
	self assert: (browser pane port: #hover) value equals: #value4