tests
simpleTimeoutWithZeroDurationTest

	[ self doSomething ]
		valueWithin: 0 seconds onTimeout:
			[ self doSomethingElse ].
	