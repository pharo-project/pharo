testing
testFibonacci
	"After an idea by Jan Vrany.
	Recursively enter the cache and its access protection"
	
	| fibCache |
	fibCache := self newCache.
	fibCache 
		maximumWeight: 32;
		beThreadSafe;
		factory: [ :key |
			key < 2
				ifTrue: [ key ]
				ifFalse: [ (fibCache at: key - 1) + (fibCache at: key - 2) ] ].
	self assert: (fibCache at: 40) equals: 102334155