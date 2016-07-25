tests
testMap
	| map |
	(map := STON mapClass new)
		at: #foo put: 1;
		at: #bar put: 2.
	self assert: (self serialize: map) = '{#foo:1,#bar:2}'.
	self assert: (self serialize: STON mapClass new) = '{}'.
	map removeAll.
	map at: 'foo bar' put: #ok.
	self assert: (self serialize: map) = '{''foo bar'':#ok}'.
	map removeAll.
	map at: 123 put: 456.
	self assert: (self serialize: map) = '{123:456}'.

