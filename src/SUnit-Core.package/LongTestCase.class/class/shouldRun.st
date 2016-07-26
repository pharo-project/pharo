accessing
shouldRun
	"Return whether long tests should be run or not"
	
	RunLongTestCases ifNil: [self runLongTestCases].
	^ RunLongTestCases 