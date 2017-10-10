tests - sunit
testTearDownMethodInSUnitTestsNeedsToBeInRunningProtocol
	"The #tearDown method in SUnit test classes should be in method protocol 'running'"
	
	self assureAll: #tearDown areCategorizedIn: #running  whenSubclassOf: TestCase
