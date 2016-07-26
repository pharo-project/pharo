tests
testTargetDoesNotIterateTheNil
	| chain |
	chain := KMDispatchChain from: (KmGlobalDispatcher new) andDispatcher: (KMDispatcher new).
	chain do: [ :target | self assert: target notNil ].