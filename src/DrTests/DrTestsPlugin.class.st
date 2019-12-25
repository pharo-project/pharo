"
I am a Plugin used to execute an analysis

To run, I need a DTPluginConfiguration that contains the items I will analyse. I define the way to execute the analysis and create a result (could be a TestResult). 
"
Class {
	#name : #DrTestsPlugin,
	#superclass : #Object,
	#instVars : [
		'announcer'
	],
	#category : #'DrTests-Model'
}

{ #category : #'api - plugins' }
DrTestsPlugin class >> availablePlugins [
	^ (self allSubclasses reject: #isAbstract)
			sort: [ :a :b | a weight < b weight ]
]

{ #category : #testing }
DrTestsPlugin class >> isAbstract [
	"Should return true if the plugin is abstract and should not be instantiated."
	^ self = DrTestsPlugin
]

{ #category : #'api - accessing' }
DrTestsPlugin class >> pluginIcon [
	^ self iconNamed: #blank
]

{ #category : #'api - accessing' }
DrTestsPlugin class >> pluginName [
	"The name of the plugin to be displayed in DrTests UI."
	^ self subclassResponsibility
]

{ #category : #'api - accessing' }
DrTestsPlugin class >> pluginResultClass [
	"Returns the class that this plugin instantiate for its results."
	^ self subclassResponsibility
]

{ #category : #'api - accessing' }
DrTestsPlugin class >> weight [
	"The lighter is a plugin, the higher it is displayed in the drop list for plugin selection."
	^ self subclassResponsibility
]

{ #category : #api }
DrTestsPlugin >> allowMiniDrTests [
	^ false
]

{ #category : #accessing }
DrTestsPlugin >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #'configuration building' }
DrTestsPlugin >> buildConfigurationFrom: aDrTests [
	"Builds a configuration from the plugin by reading the information held by the UI (aDrTests)."
	^ DTPluginConfiguration
		items: aDrTests selectedItems
		packages: aDrTests packagesSelected
]

{ #category : #api }
DrTestsPlugin >> buildContextualMenuGroupWith: presenterIntance [
	^ CmCommandGroup new
]

{ #category : #accessing }
DrTestsPlugin >> firstListLabel [
	^ 'items'
]

{ #category : #api }
DrTestsPlugin >> itemsToBeAnalysedFor: packagesSelected [
	"This method returns the items to be analysed by the plugin.
	For most plugins it will be TestCases but some plugins will return other objects like comments."

	^ packagesSelected
		flatCollect:
			[ :p | p definedClasses select: [ :c | c allSuperclasses includes: TestCase ] ]
]

{ #category : #api }
DrTestsPlugin >> packagesAvailableForAnalysis [
	^ RPackage organizer packages
		select: [ :p | p definedClasses anySatisfy: [ :c | c isTestCase ] ]
]

{ #category : #api }
DrTestsPlugin >> pluginName [
	^ self class pluginName
]

{ #category : #accessing }
DrTestsPlugin >> pluginResultClass [
	^ self class pluginResultClass
]

{ #category : #accessing }
DrTestsPlugin >> pragmaForResultTrees [
	"Returns the selector of the pragma to gather result trees."
	^ self subclassResponsibility
]

{ #category : #api }
DrTestsPlugin >> reRunForConfiguration: aDTpluginConfiguration [
]

{ #category : #api }
DrTestsPlugin >> reRunPlugin: aSymbol with: aDTpluginConfiguration [
	self deprecated: 'Should not be used, meta-programming. Need to be removed.'.
]

{ #category : #api }
DrTestsPlugin >> resultButtonHelp [
	^ self subclassResponsibility
]

{ #category : #accessing }
DrTestsPlugin >> resultTreeViews [
	"Return result tree views available.
	 This method might signal a deprecation exception in case it found an old pragma BUT the method itself is
	 NOT deprecated. Do not remove it.
	 The code generating the deprecation exception will be removed once we decide to totally remove the old pragmas support.
	"
	| pragmas oldPragmas |
	pragmas := Pragma allNamed: self pragmaForResultTrees in: self pluginResultClass.
	"Note The two following statements need to be removed once the old pragmas are gone."
	oldPragmas := Pragma allNamed: self oldPragmaForResultTrees in: self pluginResultClass.
	oldPragmas ifNotEmpty: [ self deprecated: ('Your code still use the old pragma to declare a tree view. Please update it to use the new pragma described in {1}>>#pragmaForResultTrees' format: { self className }) ].
	^ ((pragmas sorted: [ :p | p argumentAt: 2 ] asSortFunction) collect: [ :pragma |
		DTResultTreeView name: (pragma argumentAt: 1) blockToExtractViewFromResult: pragma methodSelector ]) ,
	"Note: The following code need to be removed once the old pragmas are gone."
	(oldPragmas collect: [ :pragma |
		DTResultTreeView name: (pragma argumentAt: 1) blockToExtractViewFromResult: pragma methodSelector ])
]

{ #category : #api }
DrTestsPlugin >> runForConfiguration: aDTpluginConfiguration [
	self subclassResponsibility
]

{ #category : #accessing }
DrTestsPlugin >> secondListLabel [
	^ 'items'
]

{ #category : #api }
DrTestsPlugin >> setSelectionModeOfItemsList: aListPresenter [
	aListPresenter beMultipleSelection
]

{ #category : #api }
DrTestsPlugin >> setSelectionModeOfPackageList: aListPresenter [
	aListPresenter beMultipleSelection
]

{ #category : #api }
DrTestsPlugin >> startButtonHelp [
	^ self subclassResponsibility
]

{ #category : #api }
DrTestsPlugin >> startButtonLabel [
	^ self subclassResponsibility
]

{ #category : #api }
DrTestsPlugin >> title [
	^ self pluginName
]
