Extension { #name : #TraitDescription }

{ #category : #'*NautilusCommon' }
TraitDescription >> correspondingTestCase [
	"Return the TestCase that corresponds to me.
	If it does not exist, return nil."
	
	^ NautilusUtils correspondingTestCaseOf: self
]
