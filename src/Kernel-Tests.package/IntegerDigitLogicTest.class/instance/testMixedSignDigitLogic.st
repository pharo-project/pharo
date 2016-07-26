tests
testMixedSignDigitLogic
	"Verify that mixed sign logic with large integers works."
	self assert: (-2 bitAnd: 16rFFFFFFFF) = 16rFFFFFFFE