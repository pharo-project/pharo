tests
testBuffering
	| morph flag category event1 event2 event3|
	category := KMCategory named: #TestBlah.
	KMRepository default addCategory: category.
	
	morph := BorderedMorph new.
	morph kmDispatcher reset.
	flag := false.

	category addKeymapEntry: (KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [flag := true]).
	category addKeymapEntry: (KMKeymap named: #Bar shortcut: $p asKeyCombination, $p asKeyCombination action: []).
	morph attachKeymapCategory: #TestBlah.

	event1 := self eventKey: $a.
	morph kmDispatcher
		dispatchKeystroke: event1.
	self assert: morph kmDispatcher buffer size equals: 1.
	self assert: morph kmDispatcher buffer first equals: event1.
	
	event2 := self eventKey: $b.
	morph kmDispatcher
		dispatchKeystroke: event2.
	self assert: morph kmDispatcher buffer size = 2.
	self assert: morph kmDispatcher buffer first = event1.
	self assert: morph kmDispatcher buffer second = event2.
	
	event3 := self eventKey: $c.
	morph kmDispatcher
		dispatchKeystroke: event3.
	self assert: morph kmDispatcher buffer isEmpty.
	
	self assert: flag.