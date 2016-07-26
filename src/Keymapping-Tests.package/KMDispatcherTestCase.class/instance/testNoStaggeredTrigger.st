tests
testNoStaggeredTrigger
	"Once a key sequence is recognized by a keymapper, all other  
	keymappers should clear their buffers"
	| bm1 bm2 flag1 flag2 category otherCategory bufferBefore |
	
	category := KMCategory named: #TestBlah.
	otherCategory := KMCategory named: #TestFoo.
	
	KMRepository default addCategory: category.
	KMRepository default addCategory: otherCategory.
	
	bm1 := BorderedMorph new.
	bm1 attachKeymapCategory: category.
	
	flag1 := false.
	category addKeymapEntry: ( KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [flag1 := true]).

	bm2 := Morph new.
	bm2 attachKeymapCategory: otherCategory.
	flag2 := false.

	otherCategory addKeymapEntry: (KMKeymap named: #Bar shortcut: $a asKeyCombination, $b asKeyCombination action:  [flag2 := true]).
	bm1 addMorphBack: bm2.
	bufferBefore := bm2 kmDispatcher buffer copy.
	{self eventKey: $a. self eventKey: $b. self eventKey: $c}
		do: [:e | bm2 dispatchKeystrokeForEvent: e].
	flag1 ifTrue: [ bufferBefore inspect ].
	self deny: flag1.
	self assert: flag2.