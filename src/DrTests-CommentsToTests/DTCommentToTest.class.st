"
I am a DrTestPlugin.
I create tests from executable comments and run these tests.
"
Class {
	#name : #DTCommentToTest,
	#superclass : #DrTestsPlugin,
	#category : #'DrTests-CommentsToTests'
}

{ #category : #'api - accessing' }
DTCommentToTest class >> pluginName [
	"The name of the plugin to be displayed in DrTests UI."
	^ 'Executable comments checker'
]

{ #category : #'api - accessing' }
DTCommentToTest class >> pluginResultClass [
	^ DTCommentToTestResult
]

{ #category : #'api - accessing' }
DTCommentToTest class >> weight [
	"The lighter is a plugin, the higher it is displayed in the drop list for plugin selection."
	^ 10
]

{ #category : #'configuration building' }
DTCommentToTest >> buildConfigurationFrom: aDrTests [
	^ DTCommentTestConfiguration items: aDrTests selectedItems packages: aDrTests packagesSelected 
]

{ #category : #api }
DTCommentToTest >> firstListLabel [
	^ 'Packages'
]

{ #category : #api }
DTCommentToTest >> itemsToBeAnalysedFor: packagesSelected [
	^ packagesSelected
		flatCollect: [ :p | 
			p definedClasses
				select: [ :c | 
					c methods
						anySatisfy: [ :m | 
							(m methodClass compiledMethodAt: m selector) comments
								anySatisfy: [ :com | com includesSubstring: '>>>' ] ] ] ]
]

{ #category : #api }
DTCommentToTest >> packagesAvailableForAnalysis [
	^ RPackage organizer packages
		select: [ :p | p definedClasses anySatisfy: [ :c | c isTestCase not ] ]
]

{ #category : #accessing }
DTCommentToTest >> pragmaForResultTrees [
	^ #'dtCommentToTestResultTreeNamed:order:'
]

{ #category : #api }
DTCommentToTest >> resultButtonHelp [
	^ 'Browse the test selected in the result list.'
]

{ #category : #api }
DTCommentToTest >> runForConfiguration: aDTpluginConfiguration [
	^ self pluginResultClass new
			testResults: (self runTestSuites: {aDTpluginConfiguration asTestSuite});
			yourself
]

{ #category : #api }
DTCommentToTest >> runSuite: aTestSuite withResult: aResult [
	aTestSuite
		when: TestAnnouncement
		do: [ :testAnnouncement | 
			self announcer
				announce:
					(DTStatusUpdate
						message: ('Running test {1}.' format: {testAnnouncement test asString})) ].
	[ aTestSuite run: aResult ]
		ensure: [ aTestSuite unsubscribe: TestAnnouncement ]
]

{ #category : #api }
DTCommentToTest >> runTestSuites: testSuites [
	| result |
	result := TestAsserter classForTestResult new.
	CurrentExecutionEnvironment
		runTestsBy: [ testSuites
				do: [ :testSuite | self runSuite: testSuite withResult: result ]
				displayingProgress: 'Running Tests' ].
	^ result
]

{ #category : #api }
DTCommentToTest >> secondListLabel [
	^ 'Classes'
]

{ #category : #api }
DTCommentToTest >> startButtonHelp [
	^ 'Run selected tests.' 
]

{ #category : #api }
DTCommentToTest >> startButtonLabel [
	^ 'Run Tests' translated
]
