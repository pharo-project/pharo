"
I am an editor of class definition.
I allow to modify class properties (package, tags and variables) using text editor.

And in addition I allow to create new classes by typing new class name
"
Class {
	#name : #ClyClassDefinitionEditorToolMorph,
	#superclass : #ClyClassEditorToolMorph,
	#category : #'Calypso-SystemTools-Core-Editors-Classes'
}

{ #category : #operations }
ClyClassDefinitionEditorToolMorph >> applyChanges [
	| newClass |
	newClass := browser compileANewClassFrom: self pendingText asString notifying: textMorph startingFrom: editingClass.
	newClass ifNil: [ ^false ].
	
	editingClass == newClass ifFalse: [ 
		self removeFromBrowser].
	browser selectClass: newClass.
	^true
	
]

{ #category : #building }
ClyClassDefinitionEditorToolMorph >> decorateContainerTab [
	| title |
	super decorateContainerTab.
	title := editingClass name.

	editingClass isClassSide ifTrue: [ 
		title := title asText allBold asStringMorph ].

	containerTab label: title
]

{ #category : #initialization }
ClyClassDefinitionEditorToolMorph >> defaultIconName [
	^#classIcon
]

{ #category : #accessing }
ClyClassDefinitionEditorToolMorph >> editingText [
	self flag: #todo.
	
	^editingClass definitionForNautilus
]

{ #category : #building }
ClyClassDefinitionEditorToolMorph >> fillStatusBar [ 
	super fillStatusBar.
	
	statusBar addCommandItem: (ClySlotClassDefinitionSwitchMorph for: self)
]

{ #category : #initialization }
ClyClassDefinitionEditorToolMorph >> setUpModelFromContext [
	super setUpModelFromContext.

	editingClass := context selectedClassSide
]
