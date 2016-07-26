tests
testGlobalIsFirst
	| chain first |
	first := nil.
	chain := KMDispatchChain from: (KmGlobalDispatcher new) andDispatcher: (KMDispatcher new).
	chain do: [ :target | first ifNil: [ first := target ] ].
	self assert: first isGlobalDispatcher.