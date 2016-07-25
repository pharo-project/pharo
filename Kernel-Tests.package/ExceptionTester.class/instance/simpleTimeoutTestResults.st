results
simpleTimeoutTestResults

	| things |
	things := OrderedCollection new: self iterationsBeforeTimeout.

	self iterationsBeforeTimeout timesRepeat: [ things add: self  doSomethingString ].
	things add: self doSomethingElseString.

	^ things