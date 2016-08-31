tests
testTwo
	| stub |

	stub := self newStub.
	self
		assert: stub two isNil;
		assert: (stub two: #two) == stub;
		assert: stub two = #two.