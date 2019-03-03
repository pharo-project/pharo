"
A WindowPresenter is a spec presenter for a Window
"
Class {
	#name : #WindowPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'presenter',
		'initialExtent',
		'isClosedHolder',
		'isResizeable',
		'statusbarHolder',
		'toolBarHolder',
		'menuHolder',
		'centered'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
WindowPresenter class >> adapterName [

	^ #WindowAdapter
]

{ #category : #'instance creation' }
WindowPresenter class >> presenter: aPresenter [
	^ self new
		presenter: aPresenter;
		yourself
]

{ #category : #api }
WindowPresenter >> aboutText [
	"if my aboutText has not been set the fallback is:
	  - if I do have a model, use its' about text
	  - else use the behavior defined in super
	"

	^ aboutText value ifNil: [ self presenter ifNil: [ super aboutText ] ifNotNil: #aboutText ] ifNotNil: [ aboutText value ]
]

{ #category : #api }
WindowPresenter >> aboutTitle [
	"Answer the title of my About window"

	(self presenter respondsTo: #aboutTitle) ifTrue: [ ^ self presenter aboutTitle ].
	
	^ self presenter toolName
]

{ #category : #menu }
WindowPresenter >> addMenuItemsToWindowMenu: aMenu [

	self presenter addMenuItemsToWindowMenu: aMenu
]

{ #category : #private }
WindowPresenter >> addModelIn: widget withSpecLayout: aSpec [
	
	self presenter ifNil: [ ^ self ].
	self initializeWindow.
	self 
		changed: #addModelIn:withSpecLayout:
		with: { widget. aSpec }
]

{ #category : #menu }
WindowPresenter >> addModelItemsToWindowMenu: aMenu [
	self presenter addModelItemsToWindowMenu: aMenu
]

{ #category : #api }
WindowPresenter >> askOkToClose [
	^ self presenter askOkToClose
]

{ #category : #private }
WindowPresenter >> buildWithSpecLayout: aSpec [
	"Build the widget using the spec name provided as argument"
	| widget |

	(self spec notNil and: [ self needRebuild not ])
		ifTrue: [ 
			widget := self widget ]
		ifFalse: [ 
			widget := (self adapterFrom: self retrieveDefaultSpec) widget.
			self addModelIn: widget withSpecLayout: aSpec ].
	self ensureExtentFor: widget.
	
	^ widget
]

{ #category : #api }
WindowPresenter >> cancelled [

	^ false
]

{ #category : #api }
WindowPresenter >> centered [

	centered value: true
]

{ #category : #api }
WindowPresenter >> centeredRelativeTo: aModel [

	self changed: #centeredRelativeTo: with: { aModel }
]

{ #category : #api }
WindowPresenter >> close [

	self changed: #close with: #()
]

{ #category : #private }
WindowPresenter >> defaultInitialExtent [
	
	^ 400@300
]

{ #category : #focus }
WindowPresenter >> giveFocusToNextFrom: aModel [ 

	^ self presenter giveFocusToNextFrom: aModel
]

{ #category : #focus }
WindowPresenter >> giveFocusToPreviousFrom: aModel [ 

	^ self presenter giveFocusToPreviousFrom: aModel 
]

{ #category : #testing }
WindowPresenter >> hasMenu [ 
	
	^ self menu notNil
]

{ #category : #testing }
WindowPresenter >> hasStatusBar [
	
	^ self statusBar notNil
]

{ #category : #testing }
WindowPresenter >> hasToolBar [
	
	^ self toolBar notNil
	and: [ self toolBar notEmpty ]
]

{ #category : #api }
WindowPresenter >> initialExtent [
	^ initialExtent value
		ifNil: [ self presenter
				ifNil: [ self defaultInitialExtent ]
				ifNotNil: [ :m | 
					m extent
						ifNil: [ m initialExtent ifNil: [ self defaultInitialExtent ] ] ] ]
]

{ #category : #initialization }
WindowPresenter >> initialize [

	super initialize.

	presenter := nil asValueHolder.
	initialExtent := nil asValueHolder.
	isClosedHolder := false asValueHolder.
	isResizeable := true asValueHolder.
	centered := false asValueHolder.

	menuHolder := nil asValueHolder.
	toolBarHolder := nil asValueHolder.
	statusbarHolder := nil asValueHolder.
	
	presenter whenChangedDo: [ :m | self updateTitle ].
	centered whenChangedDo: [ :m | self changed: #centered with: #() ]
]

{ #category : #initialization }
WindowPresenter >> initializeWindow [
		
	self presenter initializeWindow: self
]

{ #category : #testing }
WindowPresenter >> isCentered [

	^ centered value
]

{ #category : #testing }
WindowPresenter >> isClosed [
	^ isClosedHolder value
]

{ #category : #api }
WindowPresenter >> isDisplayed [

	^ self window ifNil: [ false ] ifNotNil: [ :theWindow | theWindow isDisplayed ]
]

{ #category : #api }
WindowPresenter >> isResizeable [

	^ isResizeable value
]

{ #category : #api }
WindowPresenter >> isResizeable: aBoolean [

	isResizeable value: aBoolean
]

{ #category : #api }
WindowPresenter >> maximize [

	self changed: #maximize with: #()
]

{ #category : #api }
WindowPresenter >> menu [

	^ menuHolder value
]

{ #category : #api }
WindowPresenter >> menu: aMenuPresenter [

	^ menuHolder value: aMenuPresenter
]

{ #category : #api }
WindowPresenter >> minimize [

	self changed: #minimize with: #()
]

{ #category : #api }
WindowPresenter >> modalRelativeTo: aWindow [

	self changed: #modalRelativeTo: with: { aWindow }
]

{ #category : #private }
WindowPresenter >> okToChange [
	^ self presenter okToChange
]

{ #category : #api }
WindowPresenter >> openModal: aWindow [ 

	self changed: #openModal: with: { aWindow }
]

{ #category : #private }
WindowPresenter >> openWithSpecLayout: aSpec [

	window value: (self buildWithSpecLayout: aSpec).
	self changed: #open with: #().
	self updateTitle.
]

{ #category : #accessing }
WindowPresenter >> presenter [
	
	^ presenter value
]

{ #category : #accessing }
WindowPresenter >> presenter: aPresenter [

	presenter value: aPresenter
]

{ #category : #private }
WindowPresenter >> rebuildWithSpecLayout: aSpec [

	self changed: #rebuildWithSpecLayout: with: { aSpec }
]

{ #category : #api }
WindowPresenter >> statusBar [

	^ statusbarHolder value
]

{ #category : #api }
WindowPresenter >> statusBar: aStatusbarPresenter [

	^ statusbarHolder value: aStatusbarPresenter
]

{ #category : #api }
WindowPresenter >> taskbarIcon [
	^ self presenter
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :pres | pres windowIcon ifNil: [ pres taskbarIcon ] ]
]

{ #category : #private }
WindowPresenter >> title [
	^ self presenter ifNil: [ super title ] ifNotNil: [ :m | m title ]
]

{ #category : #private }
WindowPresenter >> title: aString [
	self changed: #title: with: {aString}.
	self presenter title: aString
]

{ #category : #api }
WindowPresenter >> toolBar [

	^ toolBarHolder value
]

{ #category : #api }
WindowPresenter >> toolBar: aToolbarPresenter [

	^ toolBarHolder value: aToolbarPresenter
]

{ #category : #api }
WindowPresenter >> triggerCancelAction [
	"do nothing (ensure polymorphism with DialogWindow)"
]

{ #category : #api }
WindowPresenter >> triggerOkAction [
	"do nothing (ensure polymorphism with DialogWindow)"
]

{ #category : #private }
WindowPresenter >> updateTitle [
	"Update the window title"

	self title: self title
]

{ #category : #'api-events' }
WindowPresenter >> whenClosedDo: aBlock [

	isClosedHolder whenChangedDo: [:value |
		value ifTrue: [ aBlock value ] ]
]

{ #category : #updating }
WindowPresenter >> windowIsClosing [
	isClosedHolder value: true.
	self presenter windowIsClosing
]
