Class {
	#name : #ClyTestedEnvironmentPluginTest,
	#superclass : #ClyAbsractNavigationEnvironmentTest,
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
	foundClass := plugin findTestCaseCovering: ClyTestedClassMock ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassMockTest
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseCoveringTestedMetaclass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClassMock class ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassMockTest
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseCoveringUntestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyUntestedClassMock ifAbsent: [ #noResult  ].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseWithAnotherSuffixCoveringTestedClass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClass2Mock ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClass2MockTest
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCase [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClassMockTest ifAbsent: [].

	self assert: foundClass equals: ClyTestedClassMock
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
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClass2MockTest ifAbsent: [].

	self assert: foundClass equals: ClyTestedClass2Mock
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCaseWithoutSuffix [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClassMock ifAbsent: [#noResult].

	self assert: foundClass equals: #noResult
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindingTestCoveringMethod [
	{((ClyTestedClassMock >> #one) -> (ClyTestedClassMockTest >> #testOne)) . ((ClyTestedClassMock >> #one:) -> (ClyTestedClassMockTest >> #testOne)).
	((ClyTestedClassMock >> #two) -> (ClyTestedClassMockTest >> #testTwo)) . ((ClyTestedClassMock >> #two:) -> (ClyTestedClassMockTest >> #testTwo)).
	((ClyTestedClassMock >> #one:two:) -> (ClyTestedClassMockTest >> #testOneTwo)) . ((ClyTestedClassMock >> #oneTwo:) -> (ClyTestedClassMockTest >> #testOneTwo)).
	((ClyTestedClassMock >> #untestedOne) -> nil) . ((ClyTestedClassMock >> #untestedOne:) -> nil) . ((ClyTestedClassMock >> #untestedTwo) -> nil).
	((ClyTestedClassMock >> #untestedTwo:) -> nil) . ((ClyTestedClassMock >> #untestedOne:two:) -> nil) . ((ClyTestedClassMock >> #untestedOneTwo:) -> nil).
	((ClyTestedClassMock class >> #one:two:) -> (ClyTestedClassMockTest >> #testOneTwo)).
	((ClyTestedClassMock class >> #untestedOne:two:) -> nil) . ((ClyTestedClassMockTest >> #testOne) -> nil) . ((ClyTestedClassMockTest >> #testTwo) -> nil).
	((ClyTestedClassMockTest >> #testOneTwo) -> nil)} do: [ :each | self assert: (plugin findTestMethodCovering: each key ifAbsent: nil) identicalTo: each value ]
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
			self deny: (plugin normalizeSelectorForComparison: original) identicalTo: original.
			self deny: (plugin normalizeSelectorForComparison: transformed) identicalTo: transformed.
			self deny: (plugin normalizeSelectorForComparison: original) isSymbol.
			self deny: (plugin normalizeSelectorForComparison: transformed) isSymbol ]
]
