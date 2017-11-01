Extension { #name : #CompiledMethod }

{ #category : #'*NautilusCommon' }
CompiledMethod >> asTestMethod [
	"Returns the receiver if it's a TestCase test method, or its corresponding
	TestCase test method if it has one, or nil otherwise."

	^ NautilusUtils asTestMethod: self
]

{ #category : #'*NautilusCommon' }
CompiledMethod >> correspondingTestMethod [
	"Returns the corresponding TestCase test method that tests the receiver
	or nil if there isn't one."

	^ NautilusUtils correspondingTestMethodOf: self
]

{ #category : #'*NautilusCommon' }
CompiledMethod >> correspondingTestedMethods [
	"Returns the methods tested by the receiver if it is a TestCase test method."

	^ NautilusUtils correspondingTestedMethodsOf: self
]
