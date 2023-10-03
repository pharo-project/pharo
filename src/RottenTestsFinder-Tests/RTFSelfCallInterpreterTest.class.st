"
A RTFSelfCallInterpreterTest is a test class for testing the behavior of RTFSelfCallInterpreter
"
Class {
	#name : 'RTFSelfCallInterpreterTest',
	#superclass : 'TestCase',
	#category : 'RottenTestsFinder-Tests',
	#package : 'RottenTestsFinder-Tests'
}

{ #category : 'running' }
RTFSelfCallInterpreterTest >> setUp [
	"See RTFFakeTestSuperClass comment for more information."
	super setUp.
	RTFFakeTestSuperClass superclass: TestCase
]

{ #category : 'running' }
RTFSelfCallInterpreterTest >> tearDown [
	RTFFakeTestSuperClass superclass: Object.
	super tearDown
]

{ #category : 'tests' }
RTFSelfCallInterpreterTest >> testCallingSuperHelper [
	| interpreter |
	interpreter := RTFSelfCallInterpreter new.

	interpreter considerClassesThat: [ :class | class inheritsFrom: TestAsserter ].

	interpreter send: #testCallingSuperHelper fromClass: RTFFakeTestClass.

	self
		assertCollection: interpreter selfSentMethods hasSameElements: { RTFFakeTestSuperClass >> #superAssertHelper }
]

{ #category : 'tests' }
RTFSelfCallInterpreterTest >> testSimple [
	| interpreter |
	interpreter := RTFSelfCallInterpreter new.

	interpreter considerClassesThat: [ :class | class inheritsFrom: TestAsserter ].

	interpreter send: #testFailingButNotConsideredAsRotten fromClass: RTFFakeTestClass.

	self
		assert: interpreter selfSentMethods isEmpty
]

{ #category : 'tests' }
RTFSelfCallInterpreterTest >> testWithHelper [
	| interpreter |
	interpreter := RTFSelfCallInterpreter new.

	interpreter considerClassesThat: [ :class | class inheritsFrom: TestAsserter ].

	interpreter send: #testWithAssertionInHelperRun fromClass: RTFFakeTestClass.

	self
		assertCollection: interpreter selfSentMethods hasSameElements: { RTFFakeTestClass >> #assertHelper }
]

{ #category : 'tests' }
RTFSelfCallInterpreterTest >> testWithHelperHelper [
	| interpreter |
	interpreter := RTFSelfCallInterpreter new.

	interpreter considerClassesThat: [ :class | class inheritsFrom: TestAsserter ].

	interpreter send: #testWithHelperHelper fromClass: RTFFakeTestClass.

	self
		assertCollection: interpreter selfSentMethods hasSameElements: { RTFFakeTestClass >> #helperHelper. RTFFakeTestClass >> #assertHelper }
]

{ #category : 'tests' }
RTFSelfCallInterpreterTest >> testWithSuperHelper [
	| interpreter |
	interpreter := RTFSelfCallInterpreter new.

	interpreter considerClassesThat: [ :class | class inheritsFrom: TestAsserter ].

	interpreter send: #testWithAssertionInSuperHelperRun fromClass: RTFFakeTestClass.

	self
		assertCollection: interpreter selfSentMethods hasSameElements: { RTFFakeTestSuperClass >> #superAssertHelper }
]
