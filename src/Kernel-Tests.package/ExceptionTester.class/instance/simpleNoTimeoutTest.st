tests
simpleNoTimeoutTest

	[ self doSomething ]
		valueWithin: 1 day onTimeout:
			[ self doSomethingElse ].
	