Class {
	#name : #ClyTestedEnvironmentPluginTest,
	#superclass : #ClyNavigationEnvironmentTestCase,
	#instVars : [
		'plugin'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries-Tests'
}

{ #category : #running }
ClyTestedEnvironmentPluginTest >> setUpEnvironment [
	super setUpEnvironment.
	
	environment system: ClySystemEnvironment currentImage.
	plugin := ClyTestedEnvironmentPlugin new.
	plugin environment: environment.
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseCoveringTestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassTest
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseCoveringTestedMetaclass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass class ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassTest
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseCoveringUntestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyUntestedClass ifAbsent: [ #noResult  ].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseWithAnotherSuffixCoveringTestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass2 ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClass2Test
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCase [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClassTest ifAbsent: [].

	self assert: foundClass equals: ClyTestedClass
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCaseWhenItAbsent [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyAbstractlyNamedTest ifAbsent: [#noResult].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCaseWithAnotherSuffix [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClass2Test ifAbsent: [].

	self assert: foundClass equals: ClyTestedClass2
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCaseWithoutSuffix [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClass ifAbsent: [#noResult].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindingTestCoveringMethod [
	{((ClyTestedClass >> #one) -> (ClyTestedClassTest >> #testOne)).
	((ClyTestedClass >> #one:) -> (ClyTestedClassTest >> #testOne)).
	((ClyTestedClass >> #two) -> (ClyTestedClassTest >> #testTwo)).
	((ClyTestedClass >> #two:) -> (ClyTestedClassTest >> #testTwo)).
	((ClyTestedClass >> #one:two:) -> (ClyTestedClassTest >> #testOneTwo)).
	((ClyTestedClass >> #oneTwo:) -> (ClyTestedClassTest >> #testOneTwo)).
	((ClyTestedClass >> #untestedOne) -> nil).
	((ClyTestedClass >> #untestedOne:) -> nil).
	((ClyTestedClass >> #untestedTwo) -> nil).
	((ClyTestedClass >> #untestedTwo:) -> nil).
	((ClyTestedClass >> #untestedOne:two:) -> nil).
	((ClyTestedClass >> #untestedOneTwo:) -> nil).
	((ClyTestedClass class >> #one:two:) -> (ClyTestedClassTest >> #testOneTwo)).
	((ClyTestedClass class >> #untestedOne:two:) -> nil).
	((ClyTestedClassTest >> #testOne) -> nil).
	((ClyTestedClassTest >> #testTwo) -> nil).
	((ClyTestedClassTest >> #testOneTwo) -> nil)}
		do: [ :each | 
			self assert: (plugin findTestMethodCovering: each key ifAbsent: nil) == each value ]
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testNormalizingSelectorForComparison [
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
