tests - testing
testIfNotNilDo

	nil ifNotNil: [
		self fail: 'should not get here' ].
