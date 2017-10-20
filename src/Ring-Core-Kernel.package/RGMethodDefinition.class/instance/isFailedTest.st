SUnit-support
isFailedTest
	"Is the receiver a test method that failed?"

	^ self methodClass isTestCase
		and: [ self methodClass methodFailed: self selector ]