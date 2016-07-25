tests
simpleTimeoutTest

	| n |
	[1 to: 1000000 do: [ :i | n := i. self doSomething ] ]
		valueWithin: 50 milliSeconds onTimeout:
			[ self iterationsBeforeTimeout: n.
			self doSomethingElse ]