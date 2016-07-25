adding
addFailure: aTestCase
	"We cannot use self failures as that incorporates test expectations and so does not return the stored collection."

	^failures add: aTestCase