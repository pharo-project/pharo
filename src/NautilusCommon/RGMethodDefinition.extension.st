Extension { #name : #RGMethodDefinition }

{ #category : #'*NautilusCommon' }
RGMethodDefinition >> asTestMethod [
	"Returns the receiver if it's a TestCase test method, or its corresponding
	TestCase test method if it has one, or nil otherwise."

	^ (NautilusUtils asTestMethod: self) asRingDefinition
]

{ #category : #'*NautilusCommon' }
RGMethodDefinition >> containsShouldBeImplemented [

	^ self literals includes: #shouldBeImplemented
]

{ #category : #'*NautilusCommon' }
RGMethodDefinition >> correspondingTestMethod [
	"Returns the corresponding TestCase test method that tests the receiver
	or nil if there isn't one."

	^ (NautilusUtils correspondingTestMethodOf: self) asRingDefinition
]

{ #category : #'*NautilusCommon' }
RGMethodDefinition >> correspondingTestedMethods [
	"Returns the methods tested by the receiver if it is a TestCase test method."

	^ (NautilusUtils correspondingTestedMethodsOf: self)
		collect: [ :each | each asRingDefinition ]
]

{ #category : #'*NautilusCommon' }
RGMethodDefinition >> isOverride [
	| selector |
	selector := self selector.
	self methodClass allSuperclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
]
