Extension { #name : #CompiledMethod }

{ #category : #'*SUnit-Core' }
CompiledMethod >> hasNoAssertionOnPurpose [
	"Returns true if the method has the <noAssertionOnPurpose> pragma.
	Else returns false.
	
	If a method has such pragma, it means that it is ok that it does not perform an assertion message."
	^ self hasPragmaNamed: #noAssertionOnPurpose
]

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
CompiledMethod >> isStaticallyCallingOneOfTheAssertMethods [
	"If, with a static analysis I am calling one of the methods in the TestAssert's 'asserting' protocol, returns true.
	Else, returns false."
	^ (self messages intersection: ((TestAsserter methodsInProtocol: 'asserting') collect: #selector)) isNotEmpty
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
