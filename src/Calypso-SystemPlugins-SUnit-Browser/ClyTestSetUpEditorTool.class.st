"
I am special method editor which operates on #setUp method.
So I am appears only on TestCase's and edit most relevant #setUp method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	testClass:		<TestCase>
"
Class {
	#name : #ClyTestSetUpEditorTool,
	#superclass : #ClyMethodCodeEditorToolMorph,
	#instVars : [
		'testClass'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser'
}

{ #category : #activation }
ClyTestSetUpEditorTool class >> browserTabActivation [
	"This declaration specifies that in any browser when classes are selected, a test setup editor will be available in a tab."
	
	<classAnnotation>
	^ ClyTabActivationStrategy for: ClyClass asCalypsoItemContext
]

{ #category : #testing }
ClyTestSetUpEditorTool class >> shouldBeActivatedInContext: aBrowserContext [
	aBrowserContext isClassSelected ifFalse: [ ^false ].
	
	(aBrowserContext lastSelectedItem hasProperty: ClyTestResultProperty) ifFalse: [ ^false ].
	
	(aBrowserContext lastSelectedClass lookupSelector: #setUp) ifNil: [ ^false ].
	"last condition is to not show this tool if user selects #setUp method manually"
	aBrowserContext isMethodSelected ifFalse: [ ^true ].	
	^aBrowserContext browser methodSelection lastSelectedItem name ~= 'setUp'
]

{ #category : #accessing }
ClyTestSetUpEditorTool class >> tabOrder [
	^ ClyClassDefinitionEditorToolMorph tabOrder + 1
]

{ #category : #accessing }
ClyTestSetUpEditorTool >> activationPriority [
	^-100
]

{ #category : #testing }
ClyTestSetUpEditorTool >> belongsToCurrentBrowserContext [
	^browser isClassSelected: testClass
]

{ #category : #initialization }
ClyTestSetUpEditorTool >> defaultTitle [
	^'setUp'
]

{ #category : #accessing }
ClyTestSetUpEditorTool >> editingMethod: aMethod [
	editingMethod := aMethod
]

{ #category : #testing }
ClyTestSetUpEditorTool >> isSimilarTo: anotherBrowserTool [
	^self class = anotherBrowserTool class
		and: [ testClass == anotherBrowserTool testClass ]
]

{ #category : #initialization }
ClyTestSetUpEditorTool >> setUpModelFromContext [
	
	testClass := context lastSelectedClass
]

{ #category : #initialization }
ClyTestSetUpEditorTool >> setUpParametersFromModel [

	editingMethod := testClass lookupSelector: #setUp.
	self updateMethodTagsAndPackage
]

{ #category : #controlling }
ClyTestSetUpEditorTool >> switchToMethod: aMethod [

	browser selectMethod: aMethod.
	self removeFromBrowser
]

{ #category : #accessing }
ClyTestSetUpEditorTool >> testClass [
	^ testClass
]
