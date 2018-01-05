Extension { #name : #RG2MethodDefinition }

{ #category : #'*NautilusCommon' }
RG2MethodDefinition >> asTestMethod [
	"Returns the receiver if it's a TestCase test method, or its corresponding
	TestCase test method if it has one, or nil otherwise."

	^ (NautilusUtils asTestMethod: self) asRingDefinition
]

{ #category : #'*NautilusCommon' }
RG2MethodDefinition >> containsShouldBeImplemented [

	^ self ast sentMessages includes: #shouldBeImplemented

]

{ #category : #'*NautilusCommon' }
RG2MethodDefinition >> correspondingTestMethod [
	"Returns the corresponding TestCase test method that tests the receiver
	or nil if there isn't one."

	^ (NautilusUtils correspondingTestMethodOf: self) asRingDefinition
]

{ #category : #'*NautilusCommon' }
RG2MethodDefinition >> correspondingTestedMethods [
	"Returns the methods tested by the receiver if it is a TestCase test method."

	^ (NautilusUtils correspondingTestedMethodsOf: self)
		collect: [ :each | each asRingDefinition ]
]

{ #category : #'*NautilusCommon' }
RG2MethodDefinition >> isOverride [
	| selector |
	selector := self selector.
	self parent allSuperclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
]
