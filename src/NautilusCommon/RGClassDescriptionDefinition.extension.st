Extension { #name : #RGClassDescriptionDefinition }

{ #category : #'*NautilusCommon' }
RGClassDescriptionDefinition >> correspondingTestCase [
	"Return the TestCase that corresponds to me.
	If it does not exist, return nil."

	^ (NautilusUtils correspondingTestCaseOf: self) asRingDefinition
]

{ #category : #'*NautilusCommon' }
RGClassDescriptionDefinition >> extendingPackages [

	^ self realClass extendingPackages
]
