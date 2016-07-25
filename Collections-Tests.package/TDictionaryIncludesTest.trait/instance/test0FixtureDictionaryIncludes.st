tests - fixture
test0FixtureDictionaryIncludes
	| in |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self valueNotInNonEmpty.
	in := false.
	self nonEmpty
		valuesDo: [ :assoc | 
			assoc = self valueNotInNonEmpty
				ifTrue: [ in := true ] ].
	self assert: in = false.
	self keyNotInNonEmpty.
	in := false.
	self nonEmpty
		keysDo: [ :assoc | 
			assoc = self keyNotInNonEmpty
				ifTrue: [ in := true ] ].
	self assert: in = false