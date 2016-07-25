tests
testValueWithinNonLocalReturnFixSimply
	"self run: #testValueWithinNonLocalReturnFixSimply"

	"The simple version to test the fix"

	self valueWithinNonLocalReturn.
	(Delay forMilliseconds: 50) wait