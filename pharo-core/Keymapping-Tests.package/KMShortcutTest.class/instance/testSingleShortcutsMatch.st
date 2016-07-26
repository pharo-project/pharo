tests
testSingleShortcutsMatch
	|a one home |
	a := self eventKey: $a.
	one := self eventKey: 1 asCharacter.
	home := self eventKey: Character home.
	
	self assert: ($a asKeyCombination matches: {a}).
	self assert: (1 asKeyCombination matches: {one}).
	self assert: (Character home asKeyCombination matches: {home}).
	
	self deny: ($b asKeyCombination matches: {a}).
	
	self assert: ($a asKeyCombination matchesCompletely: {a}).
	self assert: (1 asKeyCombination matchesCompletely: {one}).