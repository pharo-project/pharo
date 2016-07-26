tests
testAssociation
	self assert: (self serialize: 'foo' -> 1) =  '''foo'':1'.
	self assert: (self serialize: #bar -> 2) =  '#bar:2'.
	self assert: (self serialize: 'foo bar' -> #ok) =  '''foo bar'':#ok'.
	self assert: (self serialize: 123 -> 456) =  '123:456'
