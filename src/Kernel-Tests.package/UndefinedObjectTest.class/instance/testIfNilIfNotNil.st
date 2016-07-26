tests - testing
testIfNilIfNotNil

	self should: [ nil ifNil: [self halt] ifNotNil: [self error] ] raise: Halt.


