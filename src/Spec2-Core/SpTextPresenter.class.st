"
I handle basic multi-line text.
See AbstractWidgetPresenter

self example

@@TODO

!! Transmissions
- #inputTextPort (default)
- #outputTextChangedPort (default)
"
Class {
	#name : #SpTextPresenter,
	#superclass : #SpAbstractTextPresenter,
	#traits : 'SpTHaveWrappingScrollBars',
	#classTraits : 'SpTHaveWrappingScrollBars classTrait',
	#instVars : [
		'#scrollValue => SpObservableSlot',
		'#editionContextMenu => SpObservableSlot',
		'#editable => SpObservableSlot',
		'#cursorPosition => SpObservableSlot',
		'#wrapWord => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Text'
}

{ #category : #specs }
SpTextPresenter class >> adapterName [

	^ #TextAdapter
]

{ #category : #commands }
SpTextPresenter class >> buildEditionCommandsGroupFor: presenterInstance [
	| rootCommandGroup |

	rootCommandGroup := CmCommandGroup forSpec beRoot.
	self 
		buildEditionCommandsGroupWith: presenterInstance 
		forRoot: rootCommandGroup.
	
	^ rootCommandGroup
]

{ #category : #commands }
SpTextPresenter class >> buildEditionCommandsGroupWith: presenter forRoot: aCmCommandsGroup [

	aCmCommandsGroup beDisplayedAsGroup.
	aCmCommandsGroup register: (self textEditionCommandsGroupWith: presenter)
]

{ #category : #commands }
SpTextPresenter class >> textEditionCommandsGroupWith: aPresenter [
	| group |

	group := (CmCommandGroup named: 'Selection edition commands') asSpecGroup.
	group
		description: 'Edit code selection.';
		beDisplayedAsGroup.
	
	SpTextEditionCommand allSubclassesDo: [ :commandClass | 
		group register: (commandClass forSpecContext: aPresenter) ].
	
	^ group
]

{ #category : #specs }
SpTextPresenter class >> title [

	^ 'Text'
]

{ #category : #api }
SpTextPresenter >> beEditable [

	self editable: true
]

{ #category : #api }
SpTextPresenter >> beNotEditable [

	self editable: false
]

{ #category : #api }
SpTextPresenter >> beNotWrapWord [

	self wrapWord: false
]

{ #category : #api }
SpTextPresenter >> beWrapWord [

	self wrapWord: true
]

{ #category : #api }
SpTextPresenter >> cursorPosition [
	"Answer a Point (column, row) with current cursor position"

	^ self withAdapterDo: [ :anAdapter | anAdapter cursorPosition ]
]

{ #category : #api }
SpTextPresenter >> cursorPositionIndex: index [
	"Sets cursor position index"

	^ self withAdapterDo: [ :anAdapter | anAdapter cursorPositionIndex: index ]
]

{ #category : #accessing }
SpTextPresenter >> editable: aBoolean [

	editable := aBoolean
]

{ #category : #private }
SpTextPresenter >> editionCommandsGroup [

	^ SpRecursiveContextSetter 
		visit: (self class buildEditionCommandsGroupFor: self defaultCommandsContext) 
		toSetContext: self defaultCommandsContext
]

{ #category : #accessing }
SpTextPresenter >> editionContextKeyBindings [

	^ self editionCommandsGroup asKMCategory
]

{ #category : #accessing }
SpTextPresenter >> editionContextMenu [

	^ self editionCommandsGroup asMenuPresenter
]

{ #category : #testing }
SpTextPresenter >> hasEditionContextMenu [

	^ editionContextMenu
]

{ #category : #initialization }
SpTextPresenter >> initialize [ 

	super initialize.
	self initializeTHaveWrappingScrollBars.
	self flag: #REVIEW. "We really need scrollValue?"
	scrollValue := 0@0.
	self beEditable.
	self beWrapWord.
	self withScrollBars.
	self withEditionContextMenu.
]

{ #category : #testing }
SpTextPresenter >> isEditable [

	^ editable
]

{ #category : #testing }
SpTextPresenter >> isWrapWord [

	^ wrapWord
]

{ #category : #initialization }
SpTextPresenter >> registerEvents [

	super registerEvents.
	self whenScrollValueChangedDo: [ :newPosition | 
		self changed: #setScrollValue: with: { newPosition } ]
]

{ #category : #api }
SpTextPresenter >> scrollValue [
	"Return the current scroll position"

	^ scrollValue
]

{ #category : #api }
SpTextPresenter >> scrollValue: aPoint [
	"Set the scroll position"

	^ scrollValue := aPoint
]

{ #category : #'api-events' }
SpTextPresenter >> whenCursorPositionChangedDo: aBlock [

	self property: #cursorPosition whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTextPresenter >> whenEditableChangedDo: aBlock [

	self property: #editable whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTextPresenter >> whenScrollValueChangedDo: aBlock [
	"Set a block to perform when the scroll value changed"
	
	self property: #scrollValue whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTextPresenter >> whenSubmitDo: aBlock [
	"This method will add a 'submit' event to the widget. It will react when user presses meta+s 
	 key (this is for historical reasons) and with enter."

	self 
		bindKeyCombination: $s meta 
		toAction: [ aBlock value: self text ]
]

{ #category : #'api-events' }
SpTextPresenter >> whenWrapWordChangedDo: aBlock [

	self property: #wrapWord whenChangedDo: aBlock
]

{ #category : #accessing }
SpTextPresenter >> withEditionContextMenu [

	editionContextMenu := true
]

{ #category : #accessing }
SpTextPresenter >> withoutEditionContextMenu [

	editionContextMenu := true
]

{ #category : #accessing }
SpTextPresenter >> wrapWord: aBoolean [

	wrapWord := aBoolean
]
