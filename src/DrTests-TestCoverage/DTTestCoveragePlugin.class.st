"
I am a DrTestPlugin.
I show the percentage of covered methods with tests and show the list of the uncovered methods.
"
Class {
	#name : 'DTTestCoveragePlugin',
	#superclass : 'DrTestsPlugin',
	#instVars : [
		'methods',
		'testClasses'
	],
	#category : 'DrTests-TestCoverage',
	#package : 'DrTests-TestCoverage'
}

{ #category : 'api - accessing' }
DTTestCoveragePlugin class >> pluginName [
	^ 'Test Coverage'
]

{ #category : 'api - accessing' }
DTTestCoveragePlugin class >> pluginResultClass [
	^ DTTestCoverageResult
]

{ #category : 'api - accessing' }
DTTestCoveragePlugin class >> weight [
	^ 3
]

{ #category : 'api' }
DTTestCoveragePlugin >> allowMiniDrTests [
	^ true
]

{ #category : 'api' }
DTTestCoveragePlugin >> allowMultipleSelectionInSecondList [
	^ false
]

{ #category : 'api' }
DTTestCoveragePlugin >> defineData: aDTpluginConfiguration [

	methods := aDTpluginConfiguration items flatCollect: [ :p |
		           p methods reject: [ :meth |
			           meth isTestMethod or: [meth methodClass isTestCase] ] ].
	methods := methods reject: [ :method | method isAbstract or: [ (method hasPragmaNamed: #ignoreForCoverage) ] ] .
	
	testClasses := aDTpluginConfiguration packagesSelected flatCollect: [
		               :p | p classes ]
]

{ #category : 'accessing' }
DTTestCoveragePlugin >> firstListLabel [
	^ 'Test Packages'
]

{ #category : 'api' }
DTTestCoveragePlugin >> itemsToBeAnalysedFor: packagesSelected [

	^ self packageOrganizer packages select: [ :p | p definedClasses anySatisfy: [ :c | c isTestCase not ] ]
]

{ #category : 'ui' }
DTTestCoveragePlugin >> pluginPresenterClass [

	^ DTCoveragePluginPresenter
]

{ #category : 'accessing' }
DTTestCoveragePlugin >> pragmaForResultTrees [
	^ #'dtTestCoverageResultTreeNamed:order:'
]

{ #category : 'api' }
DTTestCoveragePlugin >> resultButtonHelp [
	^ 'Browse the method selected in the results list.' translated
]

{ #category : 'api' }
DTTestCoveragePlugin >> runForConfiguration: aDTpluginConfiguration [

	| suite collector coverage notExecuted result |
	result := self pluginResultClass new.
	self defineData: aDTpluginConfiguration.
	suite := self suiteFor: testClasses.
	collector := DTCoverageCollector new.
	collector methods: methods.
	coverage := collector runOn: [ self runTestSuites: { suite } ].
	notExecuted := coverage uncoveredMethods.

	methods ifNotEmpty: [
		result
			percent: 1s2 - (notExecuted size / methods size asScaledDecimal: 2);
			methodList: notExecuted;
			partiallyCoveredMethods: coverage partiallyCoveredMethods ].
	^ result
]

{ #category : 'api' }
DTTestCoveragePlugin >> runSuite: aTestSuite withResult: aResult [

	aTestSuite when: TestAnnouncement do: [ :testAnnouncement |
		self announceStatusChanged: ('Running test {1}.' format: { testAnnouncement test asString }) ]
	for: self.
	[ aTestSuite run: aResult ] ensure: [
		aTestSuite unsubscribe: TestAnnouncement ]
]

{ #category : 'api' }
DTTestCoveragePlugin >> runTestSuites: testSuites [
	| result |
	result := TestAsserter classForTestResult new.
	CurrentExecutionEnvironment
		runTestsBy: [ testSuites
				do: [ :testSuite | self runSuite: testSuite withResult: result ]
				displayingProgress: 'Running Tests' ]
]

{ #category : 'accessing' }
DTTestCoveragePlugin >> secondListLabel [
	^ 'Package under coverage'
]

{ #category : 'api' }
DTTestCoveragePlugin >> setSelectionModeOfItemsList: aListPresenter [
	aListPresenter
		beSingleSelection;
		unselectAll
]

{ #category : 'api' }
DTTestCoveragePlugin >> startButtonHelp [
	^ 'Run test coverage in selected packages' translated
]

{ #category : 'api' }
DTTestCoveragePlugin >> startButtonLabel [
	^ 'Run Coverage' translated
]

{ #category : 'api' }
DTTestCoveragePlugin >> suiteFor: classesSelected [
	"Return the suite for all the selected test case classes"
	
	| suite |
	suite :=  TestSuite new.
	classesSelected do: [ :each |
			  each isAbstract ifFalse: [
				  each isTestCase ifTrue: [ each addToSuiteFromSelectors: suite ] ] ].
	^ suite name: 'Test'
]
