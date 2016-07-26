tests
testPassesOnTheTarget
	| chain dispatcher passed |
	dispatcher := KMDispatcher target: Morph new.
	passed := false.
	chain := KMDispatchChain from: (KmGlobalDispatcher new) andDispatcher: dispatcher.
	chain do: [ :t | t == dispatcher target ifTrue: [ passed := true ] ].
	self assert: passed