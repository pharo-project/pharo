tests
testOne
	| stub |

	stub := self newStub.
	self
		assert: stub one isNil;
		assert: (stub one: #one) == stub;
		assert: stub one = #one.