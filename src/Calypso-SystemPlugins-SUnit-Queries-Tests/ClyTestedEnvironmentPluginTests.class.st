Class {
	#name : #ClyTestedEnvironmentPluginTests,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#instVars : [
		'plugin'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyTestedEnvironmentPluginTests >> setUpEnvironment [
	super setUpEnvironment.
	
	environment system: ClySystemEnvironment currentImage.
	plugin := ClyTestedEnvironmentPlugin new.
	plugin environment: environment.
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestCaseCoveringTestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassTests
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestCaseCoveringTestedMetaclass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass class ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassTests
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestCaseCoveringUntestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyUntestedClass ifAbsent: [ #noResult  ].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestCaseWithAnotherSuffixCoveringTestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass2 ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClass2Test
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestedClassCoveredByTestCase [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClassTests ifAbsent: [].

	self assert: foundClass equals: ClyTestedClass
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestedClassCoveredByTestCaseWhenItAbsent [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyAbstractlyNamedTests ifAbsent: [#noResult].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestedClassCoveredByTestCaseWithAnotherSuffix [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClass2Test ifAbsent: [].

	self assert: foundClass equals: ClyTestedClass2
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindTestedClassCoveredByTestCaseWithoutSuffix [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClass ifAbsent: [#noResult].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testFindingTestCoveringMethod [
	{((ClyTestedClass >> #one) -> (ClyTestedClassTests >> #testOne)).
	((ClyTestedClass >> #one:) -> (ClyTestedClassTests >> #testOne)).
	((ClyTestedClass >> #two) -> (ClyTestedClassTests >> #testTwo)).
	((ClyTestedClass >> #two:) -> (ClyTestedClassTests >> #testTwo)).
	((ClyTestedClass >> #one:two:) -> (ClyTestedClassTests >> #testOneTwo)).
	((ClyTestedClass >> #oneTwo:) -> (ClyTestedClassTests >> #testOneTwo)).
	((ClyTestedClass >> #untestedOne) -> nil).
	((ClyTestedClass >> #untestedOne:) -> nil).
	((ClyTestedClass >> #untestedTwo) -> nil).
	((ClyTestedClass >> #untestedTwo:) -> nil).
	((ClyTestedClass >> #untestedOne:two:) -> nil).
	((ClyTestedClass >> #untestedOneTwo:) -> nil).
	((ClyTestedClass class >> #one:two:) -> (ClyTestedClassTests >> #testOneTwo)).
	((ClyTestedClass class >> #untestedOne:two:) -> nil).
	((ClyTestedClassTests >> #testOne) -> nil).
	((ClyTestedClassTests >> #testTwo) -> nil).
	((ClyTestedClassTests >> #testOneTwo) -> nil)}
		do: [ :each | 
			self assert: (plugin findTestMethodCovering: each key ifAbsent: nil) == each value ]
]

{ #category : #tests }
ClyTestedEnvironmentPluginTests >> testNormalizingSelectorForComparison [
	#(#one #one: #two #two: #one:two: #oneTwo: #+ #- #+-)
		with: #('one' 'one' 'two' 'two' 'onetwo' 'onetwo' '+' '-' '+-')
		do: [ :original :transformed | 
			self assert: (plugin normalizeSelectorForComparison: original) equals: transformed.
			self assert: (plugin normalizeSelectorForComparison: original asString) equals: transformed.
			self assert: (plugin normalizeSelectorForComparison: transformed) equals: transformed.

			"should always return a string copy"
			self deny: (plugin normalizeSelectorForComparison: original) == original.
			self deny: (plugin normalizeSelectorForComparison: transformed) == transformed.
			self deny: (plugin normalizeSelectorForComparison: original) isSymbol.
			self deny: (plugin normalizeSelectorForComparison: transformed) isSymbol ]
]
