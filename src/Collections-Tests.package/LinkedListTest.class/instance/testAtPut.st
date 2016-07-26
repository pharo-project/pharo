tests - sequenceable
testAtPut
	| ll |
	ll := LinkedList new.
	ll add: 1.
	ll at: 1 put: 2.
	self assert: (ll at: 1) equals: 2