adding
addPass: aTestCase
	"We cannot use self passed as that incorporates test expectations and so does not return the stored collection."

	^passed add: aTestCase