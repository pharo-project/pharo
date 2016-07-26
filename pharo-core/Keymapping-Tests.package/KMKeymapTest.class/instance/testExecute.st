tests
testExecute
	| bm km flag category |
	category := KMCategory named: #TestFoo.
	KMRepository default addCategory: category.
	bm := Morph new.
	bm attachKeymapCategory: #TestFoo.
	flag := false.
	category
		addKeymapEntry:
			(KMKeymap named: #Foo shortcut: $a asKeyCombination , $b asKeyCombination , $c asKeyCombination action: [ flag := true ]).
	category addKeymapEntry: (KMKeymap named: #Bar shortcut: $p asKeyCombination , $p asKeyCombination action: [  ]).
	{(self eventKey: $a).
	(self eventKey: $b).
	(self eventKey: $c)} do: [ :e | bm dispatchKeystrokeForEvent: e ].
	self assert: flag