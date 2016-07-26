tests
testAddKeymapToCategory
	| categoryToAdd entry |
	categoryToAdd := KMCategory named: #TestCategory.
	entry := KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [ "nothing" ].

	self assert: (categoryToAdd allEntries) size = 0.
	categoryToAdd addKeymapEntry: entry.
	self assert: (categoryToAdd allEntries) size = 1.