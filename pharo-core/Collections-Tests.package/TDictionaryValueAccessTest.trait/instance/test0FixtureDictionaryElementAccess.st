tests - fixture
test0FixtureDictionaryElementAccess
	| in |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self keyNotIn.
	in := true.
	self nonEmpty keys detect: [ :key | key = self keyNotIn ] ifNone: [ in := false ].
	self assert: in = false