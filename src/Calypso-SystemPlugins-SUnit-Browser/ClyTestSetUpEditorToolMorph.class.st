"
I am special method editor which operates on #setUp method.
So I am appears only on TestCase's and edit most relevant #setUp method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	testClass:		<TestCase>
"
Class {
	#name : #ClyTestSetUpEditorToolMorph,
	#superclass : #ClyMethodCodeEditorToolMorph,
	#instVars : [
		'testClass'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Tools'
}

{ #category : #activation }
ClyTestSetUpEditorToolMorph class >> browserTabActivation [
	"This declaration specifies that in any browser when classes are selected, a test setup editor will be available in a tab."
	
	<classAnnotation>
	^ ClyTabActivationStrategyAnnotation for: ClyClass asCalypsoItemContext
]

{ #category : #testing }
ClyTestSetUpEditorToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	aBrowserContext isClassSelected ifFalse: [ ^false ].
	
	(aBrowserContext lastSelectedItem hasProperty: ClyTestResultProperty) ifFalse: [ ^false ].
	
	(aBrowserContext lastSelectedClass lookupSelector: #setUp) ifNil: [ ^false ].
	"last condition is to not show this tool if user selects #setUp method manually"
	aBrowserContext isMethodSelected ifFalse: [ ^true ].	
	^aBrowserContext browser methodSelection lastSelectedItem name ~= 'setUp'
]

{ #category : #accessing }
ClyTestSetUpEditorToolMorph class >> tabOrder [
	^ ClyClassDefinitionEditorToolMorph tabOrder + 1
]

{ #category : #accessing }
ClyTestSetUpEditorToolMorph >> activationPriority [
	^-100
]

{ #category : #testing }
ClyTestSetUpEditorToolMorph >> belongsToCurrentBrowserContext [
	^browser isClassSelected: testClass
]

{ #category : #building }
ClyTestSetUpEditorToolMorph >> buildTextMorph [
	super buildTextMorph.
	
	editingMethod == (TestCase >> #setUp) ifTrue: [ 
		self setUpDefaultTemplate ].
]

{ #category : #initialization }
ClyTestSetUpEditorToolMorph >> defaultTemplateForNewSetUp [

	^'setUp
	super setUp.
	
	"Put here a common initialization logic for tests"'
]

{ #category : #initialization }
ClyTestSetUpEditorToolMorph >> defaultTitle [
	^'setUp'
]

{ #category : #accessing }
ClyTestSetUpEditorToolMorph >> editingMethod: aMethod [
	editingMethod := aMethod
]

{ #category : #testing }
ClyTestSetUpEditorToolMorph >> isSimilarTo: anotherBrowserTool [
	^self class = anotherBrowserTool class
		and: [ testClass == anotherBrowserTool testClass ]
]

{ #category : #initialization }
ClyTestSetUpEditorToolMorph >> setUpDefaultTemplate [

	textModel setInitialText: self defaultTemplateForNewSetUp.
	targetClasses := { testClass }
]

{ #category : #initialization }
ClyTestSetUpEditorToolMorph >> setUpModelFromContext [
	
	testClass := context lastSelectedClass
]

{ #category : #initialization }
ClyTestSetUpEditorToolMorph >> setUpParametersFromModel [

	editingMethod := testClass lookupSelector: #setUp.
	super setUpParametersFromModel
]

{ #category : #controlling }
ClyTestSetUpEditorToolMorph >> switchToMethod: aMethod [

	browser selectMethod: aMethod.
	self removeFromBrowser
]

{ #category : #accessing }
ClyTestSetUpEditorToolMorph >> testClass [
	^ testClass
]
