testing
testWithExceptionDo

	self
		should: [self error: 'foo']
		raise: self defaultTestError
		withExceptionDo: [:exception |
			self assert: (exception description includesSubstring: 'foo')
		]
			