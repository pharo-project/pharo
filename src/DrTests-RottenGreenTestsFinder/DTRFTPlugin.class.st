"
I am plugin implementing RottenGreenTestsFinder in DrTests
"
Class {
	#name : 'DTRFTPlugin',
	#superclass : 'DrTestsPlugin',
	#category : 'DrTests-RottenGreenTestsFinder-Base',
	#package : 'DrTests-RottenGreenTestsFinder',
	#tag : 'Base'
}

{ #category : 'api - accessing' }
DTRFTPlugin class >> pluginName [
	"The name of the plugin to be displayed in DrTests UI."
	^ 'Rotten green tests finder'
]

{ #category : 'api - accessing' }
DTRFTPlugin class >> pluginResultClass [
	"Returns the class that this plugin instantiate for its results."
	^ DTRTFResult
]

{ #category : 'api - accessing' }
DTRFTPlugin class >> weight [
	"The lighter is a plugin, the higher it is displayed in the drop list for plugin selection."
	^ 7
]

{ #category : 'analyze' }
DTRFTPlugin >> analyse: aTestClass [

	^ RottenTestsFinder analyze: aTestClass
]

{ #category : 'api' }
DTRFTPlugin >> analyseTestFrom: aDTpluginConfiguration [
	|result|
	result := self pluginResultClass new.
	aDTpluginConfiguration items do: [ :testClass |
		result add: (self analyse: testClass) ].
	^ result
]

{ #category : 'accessing' }
DTRFTPlugin >> firstListLabel [
	^ 'Packages'
]

{ #category : 'accessing' }
DTRFTPlugin >> pragmaForResultTrees [
	"Returns the selector of the pragma to gather result trees."
	^ 'dtTestRTFResultTreeNamed:order:'
]

{ #category : 'api' }
DTRFTPlugin >> resultButtonHelp [
	^ 'Browse the test selected in the results list.' translated
]

{ #category : 'api' }
DTRFTPlugin >> runForConfiguration: aDTpluginConfiguration [

	^ self analyseTestFrom: aDTpluginConfiguration
]

{ #category : 'accessing' }
DTRFTPlugin >> secondListLabel [
	^ 'Tests Cases'
]

{ #category : 'api' }
DTRFTPlugin >> startButtonHelp [
	^ 'Run Tests' translated
]

{ #category : 'api' }
DTRFTPlugin >> startButtonLabel [
	^ 'Run tests selected.' translated
]
