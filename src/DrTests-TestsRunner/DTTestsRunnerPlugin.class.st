"
I am the plugin implementing test runner for DrTests.
"
Class {
	#name : 'DTTestsRunnerPlugin',
	#superclass : 'DrTestsPlugin',
	#category : 'DrTests-TestsRunner-Base',
	#package : 'DrTests-TestsRunner',
	#tag : 'Base'
}

{ #category : 'api - accessing' }
DTTestsRunnerPlugin class >> pluginName [
	^ 'Tests Runner'
]

{ #category : 'api - accessing' }
DTTestsRunnerPlugin class >> pluginResultClass [
	^ DTTestsRunnerResult
]

{ #category : 'api - accessing' }
DTTestsRunnerPlugin class >> weight [
	^ 0
]

{ #category : 'api' }
DTTestsRunnerPlugin >> allowMiniDrTests [
	^ true
]

{ #category : 'configuration building' }
DTTestsRunnerPlugin >> buildConfigurationFrom: aDrTests [
	"Builds a configuration from the plugin by reading the information held by the UI (aDrTests)."
	^ DTTestsRunnerConfiguration
		items: aDrTests selectedItems
		packages: aDrTests packagesSelected
]

{ #category : 'api' }
DTTestsRunnerPlugin >> buildContextualMenuGroupWith: presenterInstance [

	^ (CmCommandGroup named: 'TestRunnerResult context menu') asSpecGroup
		description: 'Commands related to re-run a result.';
		register: (DTDebugTestCommand forSpecContext: presenterInstance) beHiddenWhenCantBeRun;
		register: (DTRerunCommand forSpecContext: presenterInstance) beHiddenWhenCantBeRun;
		beDisplayedAsGroup;
		yourself
]

{ #category : 'api' }
DTTestsRunnerPlugin >> firstListLabel [
	^ 'Packages'
]

{ #category : 'api' }
DTTestsRunnerPlugin >> handleReRunResult: rerunnedResult forConfiguration: aDTRerunConfiguration [
	| oldResult |
	oldResult := self
		removeTests: aDTRerunConfiguration configurationToRun items
		from: aDTRerunConfiguration previousResult testResults.
	^ self pluginResultClass new
		testResults: (self joinTestResult: oldResult with: rerunnedResult testResults);
		yourself
]

{ #category : 'private' }
DTTestsRunnerPlugin >> joinTestResult: aTestResult with: partialResult [
	| newResult |
	newResult := aTestResult.
	partialResult passed do: [ :each | newResult addPass: each ].
	partialResult errors do: [ :each | newResult addError: each ].
	partialResult failures do: [ :each | newResult addFailure: each ].
	partialResult skipped do: [ :each | newResult addSkip: each ].
	^ newResult
]

{ #category : 'tests' }
DTTestsRunnerPlugin >> label: aString forSuite: aTestSuite [
	^ String streamContents: [ :stream |
		stream nextPutAll: 'Running '; print: aTestSuite tests size; space; nextPutAll: aString.
		aTestSuite tests size > 1 ifTrue: [ stream nextPut: $s ] ]
]

{ #category : 'accessing' }
DTTestsRunnerPlugin >> pragmaForResultTrees [
	^ #'dtTestRunnerResultTreeNamed:order:'
]

{ #category : 'private' }
DTTestsRunnerPlugin >> removeTest: aTest from: aTestResult [
	aTestResult
		passed: (aTestResult passed copyWithout: aTest).
	aTestResult
		failures: (aTestResult failures copyWithout: aTest).
	aTestResult
		errors: (aTestResult errors copyWithout: aTest).
	^ aTestResult
]

{ #category : 'private' }
DTTestsRunnerPlugin >> removeTests: aCollectionOfTest from: aTestResult [
	aCollectionOfTest do: [ :test |
		self removeTest: test from: aTestResult ].
	^ aTestResult
]

{ #category : 'api' }
DTTestsRunnerPlugin >> resultButtonHelp [
	^ 'Browse the test selected in the results list.' 
]

{ #category : 'running' }
DTTestsRunnerPlugin >> runConfiguration: aDTpluginConfiguration [

	^ AnnouncerTestRunner runSuite: aDTpluginConfiguration asTestSuite
]

{ #category : 'api' }
DTTestsRunnerPlugin >> runForConfiguration: aDTpluginConfiguration [

	| results |
	results := self pluginResultClass new
		           testResults: (self runConfiguration: aDTpluginConfiguration);
		           yourself.
	^ aDTpluginConfiguration handleResults: results for: self
]

{ #category : 'api' }
DTTestsRunnerPlugin >> secondListLabel [
	^ 'Tests Cases'
]

{ #category : 'api' }
DTTestsRunnerPlugin >> startButtonHelp [
	^ 'Run tests selected.' 
]

{ #category : 'api' }
DTTestsRunnerPlugin >> startButtonLabel [
	^ 'Run Tests' 
]
