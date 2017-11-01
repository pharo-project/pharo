Extension { #name : #TClassDescription }

{ #category : #'*NautilusCommon' }
TClassDescription >> correspondingTestCase [
	"Return the TestCase that corresponds to me.
	If it does not exist, return nil."
	
	^ NautilusUtils correspondingTestCaseOf: self
]
