"
I am special method editor which operates on #setUp method.
So I am appears only on TestCase's and edit most relevant #setUp method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	testClass:		<TestCase>
"
Class {
	#name : #ClyTestEditorTool,
	#superclass : #ClyMethodCodeEditorToolMorph,
	#instVars : [
		'testClass'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Tools'
}

{ #category : #activation }
ClyTestEditorTool class >> browserTabActivation [
	"This declaration specifies that in any browser when classes are selected, a test setup editor will be available in a tab."
	
	<classAnnotation>
	^ ClyTabActivationStrategy for: ClyClass asCalypsoItemContext
]

{ #category : #accessing }
ClyTestEditorTool class >> selector [
	^ self subclassResponsibility
]

{ #category : #testing }
ClyTestEditorTool class >> shouldBeActivatedInContext: aBrowserContext [

	"This is a hack to make sure that this method is not called for the abstract class ClyTestEditorTool but only for its subclasses"
	ClyTestEditorTool = self ifTrue: [ ^ false ].
	
	aBrowserContext isClassSelected ifFalse: [ ^false ].
	
	(aBrowserContext lastSelectedItem hasProperty: ClyTestResultProperty) ifFalse: [ ^false ].
	
	(aBrowserContext lastSelectedClass selectors includes: self selector) ifFalse: [ ^false ].
	"last condition is to not show this tool if user selects #setUp method manually"
	aBrowserContext isMethodSelected ifFalse: [ ^true ].	
	^aBrowserContext browser methodSelection lastSelectedItem name ~= 'setUp'
]

{ #category : #accessing }
ClyTestEditorTool class >> tabOrder [
	^ ClyClassDefinitionEditorToolMorph tabOrder + 1
]

{ #category : #accessing }
ClyTestEditorTool >> activationPriority [
	^-100
]

{ #category : #testing }
ClyTestEditorTool >> belongsToCurrentBrowserContext [
	^browser isClassSelected: testClass
]

{ #category : #initialization }
ClyTestEditorTool >> defaultTitle [
	^ self class selector
]

{ #category : #accessing }
ClyTestEditorTool >> editingMethod: aMethod [
	editingMethod := aMethod
]

{ #category : #testing }
ClyTestEditorTool >> isSimilarTo: anotherBrowserTool [
	^self class = anotherBrowserTool class
		and: [ testClass == anotherBrowserTool testClass ]
]

{ #category : #initialization }
ClyTestEditorTool >> setUpModelFromContext [
	
	testClass := context lastSelectedClass
]

{ #category : #initialization }
ClyTestEditorTool >> setUpParametersFromModel [

	editingMethod := testClass methodNamed: self class selector.
	super setUpParametersFromModel
]

{ #category : #controlling }
ClyTestEditorTool >> switchToMethod: aMethod [

	browser selectMethod: aMethod.
	self removeFromBrowser
]

{ #category : #accessing }
ClyTestEditorTool >> testClass [
	^ testClass
]
