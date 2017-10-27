tests
testLocalChanges
	| changes |
	gofer
		package: 'GoferBar';
		load.
	(Smalltalk globals classNamed: #GoferBar) compile: 'foo'.
	changes := gofer localChanges.
	self assert: changes operations size = 1