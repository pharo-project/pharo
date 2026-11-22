"
Tests for ClyCommandDeclarationGenerator, on the class annotations of ClyMockAnnotatedCommand. The tests compare generated sources and do not compile them.
"
Class {
	#name : 'ClyCommandDeclarationGeneratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'generator'
	],
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'running' }
ClyCommandDeclarationGeneratorTest >> setUp [

	super setUp.
	generator := ClyCommandDeclarationGenerator forPackagesNamed: #( 'Commander-Calypso-Compatibility-Tests' ).
	generator commandPackagePrefixes: #( 'Commander-Calypso-Compatibility-Tests' )
]

{ #category : 'tests' }
ClyCommandDeclarationGeneratorTest >> testDeclarationKeepsThePlacementOfTheAnnotations [

	| sources |
	sources := generator declarationSources.
	self assert: sources size equals: 1.
	self assert: sources keys anyOne equals: ClyMockAnnotatedToolContext -> 'Commander-Calypso-Compatibility-Tests'.
	self assert: sources values anyOne lines equals: {
			'commanderCalypsoCompatibilityTestsCommandsIn: aCommandMap'.
			''.
			(String tab , '<calypsoCommands>').
			(String tab , '((aCommandMap contextMenu group: ClyExtraMenuGroup) add: ClyMockAnnotatedCommand order: 42) label: ''Mock''.').
			(String tab , 'aCommandMap shortcuts add: ClyMockAnnotatedCommand key: $e meta , $m meta') }
]

{ #category : 'tests' }
ClyCommandDeclarationGeneratorTest >> testKeySourceAnswersAnEqualKeyCombination [

	{
		$x meta.
		$d meta shift.
		($e meta , $m meta , $t meta).
		Character arrowLeft asShortcut.
		Character arrowRight alt } do: [ :keyCombination | self assert: (self class compiler evaluate: (generator keySourceOf: keyCombination)) equals: keyCombination ]
]

{ #category : 'tests' }
ClyCommandDeclarationGeneratorTest >> testOnlyConvertsAnnotationsOfStrategiesWithAnInteraction [

	self assert: generator annotations size equals: 2.
	self assert: (generator annotations allSatisfy: [ :annotation | annotation annotatedClass == ClyMockAnnotatedCommand ])
]

{ #category : 'tests' }
ClyCommandDeclarationGeneratorTest >> testSelectorIsMadeFromThePackageName [

	self assert: (generator selectorForPackageNamed: 'SystemCommands-MethodCommands') equals: #systemCommandsMethodCommandsIn:.
	self assert: (generator selectorForPackageNamed: 'Calypso-SystemTools-FullBrowser') equals: #calypsoSystemToolsFullBrowserCommandsIn:
]
