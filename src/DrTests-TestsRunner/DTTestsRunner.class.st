"
I am the plugin implementing test runner for DrTests.
"
Class {
	#name : #DTTestsRunner,
	#superclass : #DrTestsPlugin,
	#category : #'DrTests-TestsRunner'
}

{ #category : #'api - accessing' }
DTTestsRunner class >> pluginName [
	^ 'Tests Runner'
]

{ #category : #'api - accessing' }
DTTestsRunner class >> pluginResultClass [
	^ DTTestsRunnerResult
]

{ #category : #'api - accessing' }
DTTestsRunner class >> weight [
	^ 0
]

{ #category : #api }
DTTestsRunner >> allowMiniDrTests [
	^ true
]

{ #category : #'configuration building' }
DTTestsRunner >> buildConfigurationFrom: aDrTests [
	"Builds a configuration from the plugin by reading the information held by the UI (aDrTests)."
	^ DTTestsRunnerConfiguration
		items: aDrTests selectedItems
		packages: aDrTests packagesSelected
]

{ #category : #api }
DTTestsRunner >> buildContextualMenuGroupWith: presenterIntance [
	^ (CmCommandGroup named: 'TestRunnerResult context menu') asSpecGroup
		description: 'Commands related to re-run a result.';
		register:
			(DTRerunCommand forSpec context: presenterIntance)
				beHiddenWhenCantBeRun;
		register:
			(DTDebugTestCommand forSpec context: presenterIntance)
				beHiddenWhenCantBeRun;
		beDisplayedAsGroup;
		yourself
]

{ #category : #api }
DTTestsRunner >> firstListLabel [
	^ 'Packages'
]

{ #category : #api }
DTTestsRunner >> handleReRunResult: rerunnedResult forConfiguration: aDTRerunConfiguration [
	| oldResult |
	oldResult := self
		removeTests: aDTRerunConfiguration configurationToRun items
		from: aDTRerunConfiguration previousResult testResults.
	^ self pluginResultClass new
		testResults: (self joinTestResult: oldResult with: rerunnedResult testResults);
		yourself
]

{ #category : #private }
DTTestsRunner >> joinTestResult: aTestResult with: partialResult [
	| newResult |
	newResult := aTestResult.
	partialResult passed do: [ :each | newResult addPass: each ].
	partialResult errors do: [ :each | newResult addError: each ].
	partialResult failures do: [ :each | newResult addFailure: each ].
	partialResult skipped do: [ :each | newResult addSkip: each ].
	^ newResult
]

{ #category : #tests }
DTTestsRunner >> label: aString forSuite: aTestSuite [
	^ String streamContents: [ :stream |
		stream nextPutAll: 'Running '; print: aTestSuite tests size; space; nextPutAll: aString.
		aTestSuite tests size > 1 ifTrue: [ stream nextPut: $s ] ]. 
]

{ #category : #accessing }
DTTestsRunner >> pragmaForResultTrees [
	^ #'dtTestRunnerResultTreeNamed:order:'
]

{ #category : #private }
DTTestsRunner >> removeTest: aTest from: aTestResult [
	aTestResult
		passed: (aTestResult passed copyWithout: aTest).
	aTestResult
		failures: (aTestResult failures copyWithout: aTest).
	aTestResult
		errors: (aTestResult errors copyWithout: aTest).
	^ aTestResult
]

{ #category : #private }
DTTestsRunner >> removeTests: aCollectionOfTest from: aTestResult [
	aCollectionOfTest do: [ :test |
		self removeTest: test from: aTestResult ].
	^ aTestResult
]

{ #category : #api }
DTTestsRunner >> resultButtonHelp [
	^ 'Browse the test selected in the results list.' translated
]

{ #category : #api }
DTTestsRunner >> runForConfiguration: aDTpluginConfiguration [
	| results |
	results := self pluginResultClass new
		testResults: (self runTestSuites: { aDTpluginConfiguration asTestSuite });
		yourself.
	^ aDTpluginConfiguration handleResults: results for: self
]

{ #category : #private }
DTTestsRunner >> runSuite: aTestSuite withResult: aResult [
	aTestSuite
		when: TestAnnouncement
		do: [ :testAnnouncement | 
			self flag: #TODO. "Dirty"
			testAnnouncement test class = TestSuite
				ifTrue: [ self announcer
				announce:
					(DTStatusUpdate
						message: ('Running test {1}.' format: {testAnnouncement test name})) ] ].
	[ aTestSuite run: aResult ]
		ensure: [ aTestSuite unsubscribe: TestAnnouncement ]
]

{ #category : #private }
DTTestsRunner >> runTestSuites: testSuites [ 
	|result|
	result := TestAsserter classForTestResult new.
	CurrentExecutionEnvironment runTestsBy: [
		testSuites
			do: [:testSuite | self runSuite: testSuite withResult: result ]
			displayingProgress: 'Running Tests'
	].
	^ result
]

{ #category : #api }
DTTestsRunner >> secondListLabel [
	^ 'Tests Cases'
]

{ #category : #api }
DTTestsRunner >> startButtonHelp [
	^ 'Run tests selected.' translated
]

{ #category : #api }
DTTestsRunner >> startButtonLabel [
	^ 'Run Tests' translated
]

{ #category : #tests }
DTTestsRunner >> testSuiteOf: aSetOfTest withName: aString [
	^ aSetOfTest
		collect: [ :each | 
			TestSuite new
				in: [ :suite | 
					suite
						addTest: each;
						name: (self label: aString forSuite: suite) ];
				yourself ]
]
