tests
testRemoteChanges
	| changes |
	gofer
		package: 'GoferBar';
		load.
	(Smalltalk globals classNamed: #GoferBar) compile: 'foo'.
	changes := gofer remoteChanges.
	self assert: changes operations size = 1