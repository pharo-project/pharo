"
SUnit tests for the key mapping dispatcher
"
Class {
	#name : 'KMDispatcherTest',
	#superclass : 'AbstractKeymappingTest',
	#category : 'Keymapping-Tests',
	#package : 'Keymapping-Tests'
}

{ #category : 'tests' }
KMDispatcherTest >> keymapContainer [
	^ KMRepository default
]

{ #category : 'tests' }
KMDispatcherTest >> testBuffering [
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
	self assert: morph kmDispatcher buffer size equals: 2.
	self assert: morph kmDispatcher buffer first equals: event1.
	self assert: morph kmDispatcher buffer second equals: event2.

	event3 := self eventKey: $c.
	morph kmDispatcher
		dispatchKeystroke: event3.
	self assert: morph kmDispatcher buffer isEmpty.

	self assert: flag
]

{ #category : 'tests' }
KMDispatcherTest >> testDetach [

	| category1 category2 morph attachedCategories |
	category1 := KMCategory named: #TestBlah.
	category2 := KMCategory named: #TestAnother.
	morph := BorderedMorph new.
	KMRepository default addCategory: category1.
	KMRepository default addCategory: category2.
	morph kmDispatcher reset.
	morph attachKeymapCategory: #TestBlah.
	morph attachKeymapCategory: #TestAnother.
	morph kmDispatcher targets size = 2 ifFalse: [ self error: 'should have one category attached' ].

	morph detachKeymapCategory: #TestBlah.
	attachedCategories := morph kmDispatcher targets collect: [ :e | e category name ].
	self assert: attachedCategories asArray equals: { #TestAnother }.
	self should: [ morph detachKeymapCategory: #NonExistent ] raise: Error
]

{ #category : 'tests' }
KMDispatcherTest >> testNoMultiTrigger [
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
	self assert: flag2
]

{ #category : 'tests' }
KMDispatcherTest >> testNoStaggeredTrigger [
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
	self assert: flag2
]

{ #category : 'tests' }
KMDispatcherTest >> testRepeatEvents [
	| morph flag category pressA repeatPressA pressB repeatPressB pressC |
	category := KMCategory named: #TestBlah.
	KMRepository default addCategory: category.

	morph := BorderedMorph new.
	morph kmDispatcher reset.
	flag := false.

	category addKeymapEntry: (KMKeymap named: #Foo shortcut: $a asKeyCombination, $b asKeyCombination, $c asKeyCombination action: [flag := true]).
	morph attachKeymapCategory: #TestBlah.

	pressA := self eventKey: $a.
	morph kmDispatcher dispatchKeystroke: pressA.
	self assert: morph kmDispatcher buffer asArray equals: {pressA}.
	
	repeatPressA := (self eventKey: $a) isRepeat: true; yourself.
	morph kmDispatcher dispatchKeystroke: repeatPressA.
	self assert: morph kmDispatcher buffer asArray equals: {pressA. repeatPressA}.
	self assert: (morph kmDispatcher buffer asArray collect: [ :v | v isRepeat ]) equals: {false. true}.

	pressB := self eventKey: $b.
	morph kmDispatcher dispatchKeystroke: pressB.
	self assert: morph kmDispatcher buffer asArray equals: {pressA. repeatPressA. pressB}.
	self assert: (morph kmDispatcher buffer asArray collect: [ :v | v isRepeat ]) equals: {false. true. false}.

	repeatPressB := (self eventKey: $b) isRepeat: true; yourself.
	morph kmDispatcher dispatchKeystroke: repeatPressB.
	self assert: morph kmDispatcher buffer asArray equals: {pressA. repeatPressA. pressB. repeatPressB}.
	self assert: (morph kmDispatcher buffer asArray collect: [ :v | v isRepeat ]) equals: {false. true. false. true}.

	pressC := self eventKey: $c.
	morph kmDispatcher
		dispatchKeystroke: pressC.
	self assert: morph kmDispatcher buffer isEmpty.

	self assert: flag
]
