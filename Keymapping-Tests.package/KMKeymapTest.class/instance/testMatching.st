tests
testMatching
	| category blockAction a b c p|
	
	a := self eventKey: $a.
	b := self eventKey: $b.
	c := self eventKey: $c.
	p := self eventKey: $p.
	
	blockAction := [self error: 'error'].
	
	category := KMCategory named: #testCategory.
	category addKeymapEntry:  (KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: blockAction).
	category addKeymapEntry:  (KMKeymap named: #Fum shortcut: $p asKeyCombination, $p asKeyCombination action: blockAction).

	self assert: (category matches: {a}).
	self deny: (category matches: {b}).
	self assert: (category matches: {a. b}).
	self assert: (category matchesCompletely: {a. b. c}).
	self assert: (category matches: {p}).
	self assert: (category matchesCompletely: {p. p.}).
	self deny: (category matches: {a. p. p}).
	self deny: (category matches: {self eventKey: $l. self eventKey: $m. self eventKey: $n. self eventKey: $o}).