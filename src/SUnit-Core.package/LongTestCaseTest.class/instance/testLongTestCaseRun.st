testing
testLongTestCaseRun
	"self debug: #testLongTestCaseRun"
	"self run: #testLongTestCaseRun"

	LongTestCase runLongTestCases.
	LongTestCaseTestUnderTest markAsNotRun.
	self deny: LongTestCaseTestUnderTest hasRun.
	LongTestCaseTestUnderTest suite run.
	self assert: LongTestCaseTestUnderTest hasRun.
	LongTestCase doNotRunLongTestCases.

	