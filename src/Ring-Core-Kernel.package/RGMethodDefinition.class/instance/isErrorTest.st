SUnit-support
isErrorTest
	"Is the receiver a test method that raised an error?"

	^ self methodClass isTestCase
		and: [ self methodClass methodRaisedError: self selector ]