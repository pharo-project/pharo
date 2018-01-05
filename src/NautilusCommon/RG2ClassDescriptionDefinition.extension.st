Extension { #name : #RG2ClassDescriptionDefinition }

{ #category : #'*NautilusCommon' }
RG2ClassDescriptionDefinition >> correspondingTestCase [
	"Return the TestCase that corresponds to me.
	If it does not exist, return nil."

	^ (NautilusUtils correspondingTestCaseOf: self) asRingDefinition
]

{ #category : #'*NautilusCommon' }
RG2ClassDescriptionDefinition >> extendingPackages [

	^ self realClass extendingPackages
]
