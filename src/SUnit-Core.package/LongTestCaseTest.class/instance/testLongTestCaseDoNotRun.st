testing
testLongTestCaseDoNotRun
	"self debug: #testLongTestCaseDoNotRun"
	"self run: #testLongTestCaseDoNotRun"

	LongTestCase doNotRunLongTestCases.
	LongTestCaseTestUnderTest markAsNotRun.
	self deny: LongTestCaseTestUnderTest hasRun.
	LongTestCaseTestUnderTest suite run.
	self deny: LongTestCaseTestUnderTest hasRun.


	