SUnit-support
isPassedTest
	"Is the receiver a test method that passed?"

	^ self methodClass isTestCase
		and: [ self methodClass methodPassed: self selector ]