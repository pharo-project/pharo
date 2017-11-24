Extension { #name : #CompiledMethod }

{ #category : #'*SUnit-Core' }
CompiledMethod >> isErrorTest [
	"Is the receiver a test method that raised an error?"

	^ self methodClass isTestCase
		and: [ self methodClass methodRaisedError: self selector ]
]

{ #category : #'*SUnit-Core' }
CompiledMethod >> isFailedTest [
	"Is the receiver a test method that failed?"

	^ self methodClass isTestCase
		and: [ self methodClass methodFailed: self selector ]
]

{ #category : #'*SUnit-Core' }
CompiledMethod >> isPassedTest [
	"Is the receiver a test method that passed?"

	^ self methodClass isTestCase
		and: [ self methodClass methodPassed: self selector ]
]

{ #category : #'*SUnit-Core' }
CompiledMethod >> isTestMethod [

	self numArgs isZero
		ifFalse: [ ^ false ].
	"unary selectors starting with 'should' are supposed to be treated as test methods too"
	((self selector beginsWith: 'test') or: [ self selector beginsWith: 'should' ])
		ifFalse: [ ^ false ].
	"Is the receiver a TestCase test method?"
	self methodClass isTestCase
		ifFalse: [ ^ false ].
	^true
]
