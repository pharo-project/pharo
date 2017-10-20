tests
testRevert
	gofer
		package: 'GoferFoo';
		package: 'GoferBar';
		load.
	(Smalltalk globals classNamed: #GoferBar) category: 'GoferFoo'.
	gofer revert.
	self assert: (Smalltalk globals classNamed: #GoferFoo) category asSymbol = #GoferFoo.
	self assert: (Smalltalk globals classNamed: #GoferBar) category asSymbol = #GoferBar