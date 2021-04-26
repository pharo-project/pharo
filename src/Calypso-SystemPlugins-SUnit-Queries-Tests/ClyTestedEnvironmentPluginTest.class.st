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

	self assert: foundClass equals: ClyTestedClassTest
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestCaseCoveringTestedMetaclass [

	| foundClass |
	foundClass := plugin findTestCaseCovering: ClyTestedClassMock class ifAbsent: [  ].

	self assert: foundClass equals: ClyTestedClassTest
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

	self assert: foundClass equals: ClyTestedClass2Test
]

{ #category : #tests }
ClyTestedEnvironmentPluginTest >> testFindTestedClassCoveredByTestCase [

	| foundClass |
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClassTest ifAbsent: [].

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
	foundClass := plugin findTestedClassCoveredBy: ClyTestedClass2Test ifAbsent: [].

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
	{((ClyTestedClassMock >> #one) -> (ClyTestedClassTest >> #testOne)) . ((ClyTestedClassMock >> #one:) -> (ClyTestedClassTest >> #testOne)).
	((ClyTestedClassMock >> #two) -> (ClyTestedClassTest >> #testTwo)) . ((ClyTestedClassMock >> #two:) -> (ClyTestedClassTest >> #testTwo)).
	((ClyTestedClassMock >> #one:two:) -> (ClyTestedClassTest >> #testOneTwo)) . ((ClyTestedClassMock >> #oneTwo:) -> (ClyTestedClassTest >> #testOneTwo)).
	((ClyTestedClassMock >> #untestedOne) -> nil) . ((ClyTestedClassMock >> #untestedOne:) -> nil) . ((ClyTestedClassMock >> #untestedTwo) -> nil).
	((ClyTestedClassMock >> #untestedTwo:) -> nil) . ((ClyTestedClassMock >> #untestedOne:two:) -> nil) . ((ClyTestedClassMock >> #untestedOneTwo:) -> nil).
	((ClyTestedClassMock class >> #one:two:) -> (ClyTestedClassTest >> #testOneTwo)).
	((ClyTestedClassMock class >> #untestedOne:two:) -> nil) . ((ClyTestedClassTest >> #testOne) -> nil) . ((ClyTestedClassTest >> #testTwo) -> nil).
	((ClyTestedClassTest >> #testOneTwo) -> nil)} do: [ :each | self assert: (plugin findTestMethodCovering: each key ifAbsent: nil) identicalTo: each value ]
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
