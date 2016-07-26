tests
testNoMultiTrigger
	| bm1 bm2 flag1 flag2 category otherCategory |
	category := KMCategory named: #TestBlah.
	otherCategory := KMCategory named: #TestFoo.
	KMRepository default addCategory: category.
	KMRepository default addCategory: otherCategory.
	
	bm1 := BorderedMorph new.
	bm1 attachKeymapCategory: #TestBlah.
	flag1 := false.

	category addKeymapEntry: (KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [flag1 := true]).
	bm2 := Morph new.
	bm2 attachKeymapCategory: #TestFoo.
	
	flag2 := false.

	otherCategory addKeymapEntry: (KMKeymap named: #Bar shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [flag2 := true]).
	bm1 addMorphBack: bm2.
	
	{self eventKey: $a. self eventKey: $b. self eventKey: $c}
		do: [:e | bm2 dispatchKeystrokeForEvent: e].
	self deny: flag1.
	self assert: flag2.