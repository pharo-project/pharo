adding
addError: aTestCase
	"We cannot use self errors as that incorporates test expectations and so does not return the stored collection."

	^errors add: aTestCase