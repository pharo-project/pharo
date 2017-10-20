tests - sunit
testSetUpMethodInSUnitTestsNeedsToBeInRunningProtocol
	"The #setUp method in SUnit test classes should be in method protocol 'running'"

	self assureAll: #setUp areCategorizedIn: #running  whenSubclassOf: TestCase
