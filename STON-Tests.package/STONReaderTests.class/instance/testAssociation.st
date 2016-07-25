tests
testAssociation
	self assert: (self materialize: '''foo'':1') = ('foo' -> 1).
	self assert: (self materialize: '#bar:2') = (#bar -> 2).
	self assert: (self materialize: '''foo bar'':#ok') = ('foo bar' -> #ok).
	self assert: (self materialize: '123:456') = (123 -> 456).
	
	self assert: (self materialize: '''foo'' : 1') = ('foo' -> 1).
	self assert: (self materialize: '#bar : 2') = (#bar -> 2).
	self assert: (self materialize: '''foo bar'' : #ok') = ('foo bar' -> #ok).
	self assert: (self materialize: '123 : -456') = (123 -> -456).
	
	self assert: (self materialize: '#foo : 1 : 2') = (#foo -> (1 -> 2))
