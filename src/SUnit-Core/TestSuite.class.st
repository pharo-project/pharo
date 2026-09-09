"
I'm a Composite of tests, either `TestCase`s or other `TestSuite`s. The top-level protocol is `#run`. This creates a`TestResult` and sends `self run: aTestResult`.
Then ensures that any `TestResource`s made available during the run are reset.  These, and the dependencies protocol, are common between `TestSuite` and `TestCase`.

### Implementation
While I implement a composite I do not inherit from a common superclass such as we could expect. 
It is just like that. Newer versions of the frameworks may want to introduce a more conventional design. 

"
Class {
	#name : 'TestSuite',
	#superclass : 'Object',
	#instVars : [
		'tests',
		'resources',
		'name',
		'announcer',
		'randomGenerator'
	],
	#category : 'SUnit-Core-Kernel',
	#package : 'SUnit-Core',
	#tag : 'Kernel'
}

{ #category : 'instance creation' }
TestSuite class >> named: aString [

	^self new
		name: aString;
		yourself
]

{ #category : 'composing' }
TestSuite >> , aTestSuite [

		^ TestSuite new
			addTests: self tests;
			addTests: aTestSuite tests;
			yourself
]

{ #category : 'accessing' }
TestSuite >> addTest: aTest [
	self tests add: aTest
]

{ #category : 'accessing' }
TestSuite >> addTests: aCollection [
	aCollection do: [:eachTest | self addTest: eachTest]
]

{ #category : 'announcing' }
TestSuite >> announceTest: aTest [
	^ self testSuiteAnnouncer announce: (TestAnnouncement with: aTest)
]

{ #category : 'private' }
TestSuite >> cleanAfterRunOn: aTestSuite [
	"as a test suite is in fact a composite (that can have other test suites), 
	 we visit this suite in the context of its execution"

	aTestSuite cleanTestSuite: self
]

{ #category : 'private' }
TestSuite >> cleanTestCase: aTestCase [

	aTestCase class removeSubscription: self testSuiteAnnouncer
]

{ #category : 'private' }
TestSuite >> cleanTestSuite: aTestSuite [

	aTestSuite testSuiteAnnouncer: nil
]

{ #category : 'running' }
TestSuite >> debug [

	| result |
	result := TestResult new.
	self runWith: [ :test |
			[ test debug: result ] ensure: [
					result dispatchResultsIntoHistory.
					self testSuiteAnnouncer announce: (TestSuiteEnded result: result) ] ].
	^ result
]

{ #category : 'accessing' }
TestSuite >> defaultResources [
	^self tests
		inject: OrderedCollection new
		into:
			[:coll :testCase |
			testCase resources do:
				[:each |
				(coll includes: each) ifFalse: [coll add: each]].
			coll]
]

{ #category : 'initialization' }
TestSuite >> initialize [

	super initialize.
	randomGenerator := Random new
]

{ #category : 'accessing' }
TestSuite >> name [

	^name
]

{ #category : 'accessing' }
TestSuite >> name: aString [

	name := aString
]

{ #category : 'private' }
TestSuite >> prepareTestCase: aTestCase [

	aTestCase class announcer
		when: TestCaseAnnouncement 
		send: #announce: 
		to: self testSuiteAnnouncer
]

{ #category : 'private' }
TestSuite >> prepareTestSuite: aTestSuite [
	
	aTestSuite testSuiteAnnouncer: self testSuiteAnnouncer
]

{ #category : 'private' }
TestSuite >> prepareToRunOn: aTestSuite [
	"as a test suite is in fact a composite (that can have other test suites), 
	 we visit this suite in the context of its execution"

	aTestSuite prepareTestSuite: self
]

{ #category : 'accessing' }
TestSuite >> resourceClass [

	^ TestResource
]

{ #category : 'accessing' }
TestSuite >> resources [

	^ resources ifNil: [ resources := self defaultResources ]
]

{ #category : 'accessing' }
TestSuite >> resources: someOrderedTestResourceClasses [
	"The parameter should understand reverseDo: and should not contain duplicates."

	resources := someOrderedTestResourceClasses
]

{ #category : 'accessing' }
TestSuite >> resultClass [

	^ TestAsserter classForTestResult
]

{ #category : 'running' }
TestSuite >> run [
	| result |
	result := self resultClass new.
	self run: result.
	^result
]

{ #category : 'running' }
TestSuite >> run: aResult [

	CurrentExecutionEnvironment runTestsBy: [
		[ self runWith: [ :test | test run: aResult ] ]
		ensure: [
			aResult dispatchResultsIntoHistory.
			self testSuiteAnnouncer announce: (TestSuiteEnded result: aResult) ] ]
]

{ #category : 'private' }
TestSuite >> runWith: aBlock [

	self setUp.
	[ 
		self shuffledTests do: [ :each |
			"prepare for testing"
			each prepareToRunOn: self.
			[ aBlock value: each ]
			ensure: [ each cleanAfterRunOn: self ].
			self announceTest: each ] ]
	ensure: [ 
		self tearDown ]
]

{ #category : 'running' }
TestSuite >> setUp [
	self resources do: [:each |
		each isAvailable ifFalse: [ each signalInitializationError ]]
]

{ #category : 'accessing' }
TestSuite >> shuffleRandom [

	^ randomGenerator
]

{ #category : 'accessing' }
TestSuite >> shuffleRandom: aRandomGenerator [

	randomGenerator := aRandomGenerator
]

{ #category : 'accessing' }
TestSuite >> shuffleSeed [

	^ randomGenerator seed
]

{ #category : 'accessing' }
TestSuite >> shuffleSeed: aSeed [

	randomGenerator := Random seed: aSeed
]

{ #category : 'accessing' }
TestSuite >> shuffledTests [

	"Sort before shuffling so order is reproducible between different environments.

	This happens because tests are extracted from the hashed method dictionary, so the original order is not guaranteed."
	^ (self tests sorted: [ :a :b | a asString < b asString ])
			shuffledBy: self shuffleRandom
]

{ #category : 'accessing' }
TestSuite >> suite [ 
	"polymorphic method used by Test Runner"

	^ self
]

{ #category : 'running' }
TestSuite >> tearDown [
	self resourceClass resetResources: self resources
]

{ #category : 'announcing' }
TestSuite >> testSuiteAnnouncer [

	^ announcer ifNil: [ announcer := Announcer new. ]
]

{ #category : 'tests' }
TestSuite >> testSuiteAnnouncer: anAnnouncer [

	announcer := anAnnouncer
]

{ #category : 'accessing' }
TestSuite >> tests [

	^ tests ifNil: [ tests := OrderedCollection new ]
]

{ #category : 'announcing' }
TestSuite >> unsubscribe: anAnnouncementClass [
	self testSuiteAnnouncer unsubscribe: anAnnouncementClass
]

{ #category : 'announcing' }
TestSuite >> when: aAnnouncement do: aBlock for: aSubscriber [

	self testSuiteAnnouncer when: aAnnouncement do: aBlock for: aSubscriber
]
