tests
testResolvePath
	"self debug: #testResolvePath"
	
	| path |
	path := Path / 'grandfather' / 'father' / 'child'.
	self 
		assert: (path resolvePath: Path / 'grandfather') 
		equals: (Path / 'grandfather').
	self 
		assert: (path resolvePath: Path / 'child')
		equals: (Path / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert:  (path resolvePath: (Path parent) / '..') canonicalize
		equals: (Path / 'grandfather')