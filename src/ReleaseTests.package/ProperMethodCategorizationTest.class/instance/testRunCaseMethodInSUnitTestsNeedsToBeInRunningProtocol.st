tests - sunit
testRunCaseMethodInSUnitTestsNeedsToBeInRunningProtocol
	"The #runCase method in SUnit test classes should be in method protocol 'running'"
	
	self assureAll: #runCase areCategorizedIn: #running  whenSubclassOf: TestCase
