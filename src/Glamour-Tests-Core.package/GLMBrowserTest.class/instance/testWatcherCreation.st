tests
testWatcherCreation
	| browser |
	browser := GLMBrowser new.
	self assert: browser watcherPane name equals: #'_watcher'.
	(browser watcherPane port: #entity) value: 42.
	self assert: (browser watcherPane port: #entity) value equals: 42