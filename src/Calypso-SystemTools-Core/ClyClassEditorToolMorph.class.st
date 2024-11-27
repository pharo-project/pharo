"
I am a base class for various class editors based on text.

My subclasses retrieve particular textual property from the class and initialize text editor with it.
My subclasses are activated in context of selected class which I keep in my editingClass variable.

Internal Representation and Key Implementation Points.

    Instance Variables
	editingClass:		<Class>
"
Class {
	#name : 'ClyClassEditorToolMorph',
	#superclass : 'ClyTextEditorToolMorph',
	#instVars : [
		'editingClass'
	],
	#category : 'Calypso-SystemTools-Core-Editors-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Classes'
}

{ #category : 'activation' }
ClyClassEditorToolMorph class >> classTabActivation [
	<classAnnotation>

	^ClyTabActivationStrategyAnnotation for: ClyClass asCalypsoItemContext
]

{ #category : 'testing' }
ClyClassEditorToolMorph class >> isAbstract [
	^self = ClyClassEditorToolMorph
]

{ #category : 'testing' }
ClyClassEditorToolMorph class >> shouldBeActivatedInContext: aBrowserContext [
	^aBrowserContext isClassSelected
]

{ #category : 'accessing' }
ClyClassEditorToolMorph class >> tabOrder [
	^ 10
]

{ #category : 'testing' }
ClyClassEditorToolMorph >> belongsToCurrentBrowserContext [

	^(browser isClassSelected: editingClass)
		or: [ browser isClassSelected: editingClass instanceSide ]
]

{ #category : 'testing' }
ClyClassEditorToolMorph >> belongsToRemovedBrowserContext [
	^editingClass isObsolete
]

{ #category : 'controlling' }
ClyClassEditorToolMorph >> detachFromSystem [

	browser system unsubscribe: self
]

{ #category : 'accessing' }
ClyClassEditorToolMorph >> editingClass [
	^editingClass
]

{ #category : 'accessing' }
ClyClassEditorToolMorph >> editingClass: anObject [
	editingClass := anObject
]

{ #category : 'formatting' }
ClyClassEditorToolMorph >> formatSourceCode [
	"Do nothing. MAybe in the future we could format the fluid class definition but for now we don't want errors"

	
]

{ #category : 'testing' }
ClyClassEditorToolMorph >> isSimilarTo: anotherBrowserTool [

	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	^editingClass = anotherBrowserTool editingClass
]

{ #category : 'testing' }
ClyClassEditorToolMorph >> isValidInContext: aClyFullBrowserContext [
	self context class = aClyFullBrowserContext class
		ifFalse: [ ^ false ].

	self context metaLevelScope = aClyFullBrowserContext metaLevelScope
		ifFalse: [ ^ false ].

	^ editingClass = aClyFullBrowserContext lastSelectedClass
		or: [  editingClass = aClyFullBrowserContext lastSelectedClass instanceSide ]
]

{ #category : 'printing' }
ClyClassEditorToolMorph >> printContext [
	^editingClass printSystemPath
]

{ #category : 'rubric interaction model' }
ClyClassEditorToolMorph >> selectedClassOrMetaClass [
	^editingClass
]

{ #category : 'initialization' }
ClyClassEditorToolMorph >> setUpModelFromContext [

	super setUpModelFromContext.
	editingClass := context lastSelectedClass
]
