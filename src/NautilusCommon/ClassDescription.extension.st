Extension { #name : #ClassDescription }

{ #category : #'*NautilusCommon' }
ClassDescription >> correspondingTestCase [
	"Return the TestCase that corresponds to me.
	If it does not exist, return nil."
	
	^ NautilusUtils correspondingTestCaseOf: self
]
