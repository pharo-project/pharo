instance creation
buildSuite

	| suite |
	suite := self suiteClass new.
	self shouldRun ifTrue: [
		self addToSuiteFromSelectors: suite].
	^suite