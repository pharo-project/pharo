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
		'centered',
		'windowHolder'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
WindowPresenter class >> adapterName [

	^ #WindowAdapter
]

{ #category : #'instance creation' }
WindowPresenter class >> application: anApplication [
	^ self new
		application: anApplication;
		yourself
]

{ #category : #accessing }
WindowPresenter class >> defaultTitle [

	^ 'Untitled window'
]

{ #category : #'instance creation' }
WindowPresenter class >> presenter: aPresenter [
	^ (self application: aPresenter application)
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

{ #category : #menu }
WindowPresenter >> addModelItemsToWindowMenu: aMenu [
	self presenter addModelItemsToWindowMenu: aMenu
]

{ #category : #'private building' }
WindowPresenter >> addPresenterIn: widget withSpecLayout: aSpec [
	
	self presenter ifNil: [ ^ self ].
	self initializeWindow.
	self 
		changed: #addPresenterIn:withSpecLayout:
		with: { widget. aSpec }
]

{ #category : #api }
WindowPresenter >> askOkToClose [

	^ askOkToClose
]

{ #category : #api }
WindowPresenter >> askOkToClose: aBoolean [

	askOkToClose := aBoolean
]

{ #category : #'private building' }
WindowPresenter >> basicBuildWithSpecLayout: presenterSpecLayout [
	"Build the widget using the spec name provided as argument"
	| widget |

	(self spec isNil or: [ self needRebuild ])
		ifTrue: [ 
			self 
				buildWindowWithLayout: self retrieveDefaultSpec 
				presenterLayout: presenterSpecLayout ]
		ifFalse: [
			widget := self widget ].
	
	^ widget
]

{ #category : #'private building' }
WindowPresenter >> buildWindowWithLayout: windowSpecLayout presenterLayout: presenterSpecLayout [

	"set adapter of the window"
	self adapter: (windowSpecLayout asSpecLayout
		buildAdapterFor: self 
		bindings: SpecBindings value).
	self window: self adapter widget.
	"add presenter"
	self addPresenterIn: self window withSpecLayout: presenterSpecLayout.
	"announce its result"
	self announce: (WidgetBuilt model: self widget: self window).
	self presenter announce: (WindowBuilt model: self).	
	
	^ self window
]

{ #category : #'private building' }
WindowPresenter >> buildWithSpecLayout: aSpecLayout [
	
	^ SpecBindings 
		value: self application adapterBindings
		during: [ self basicBuildWithSpecLayout: aSpecLayout ]
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

	"Only manage closing if the presenter is really open"
	self isOpen ifFalse: [ ^ self ].
	
	self changed: #close with: #().
	self application windowClosed: self
]

{ #category : #private }
WindowPresenter >> defaultInitialExtent [
	
	^ 400@300
]

{ #category : #private }
WindowPresenter >> delete [

	self window delete
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
WindowPresenter >> hasPresenter: aButtonPresenter [

	^ aButtonPresenter isNotNil and: [aButtonPresenter adapter notNil
		and: [ self adapter hasWidget: aButtonPresenter adapter ]]
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
		ifNil: [ self defaultInitialExtent ]
]

{ #category : #api }
WindowPresenter >> initialExtent: anExtent [

	initialExtent value: anExtent
]

{ #category : #initialization }
WindowPresenter >> initialize [

	super initialize.

	presenter := nil asValueHolder.
	windowHolder := nil asValueHolder.
	titleHolder := self class defaultTitle asValueHolder.
	initialExtent := nil asValueHolder.
	isClosedHolder := true asValueHolder.
	isResizeable := true asValueHolder.
	centered := false asValueHolder.
	askOkToClose := true.

	menuHolder := nil asValueHolder.
	toolBarHolder := nil asValueHolder.
	statusbarHolder := nil asValueHolder.
	
	"presenter whenChangedDo: [ :m | self updateTitle ]."
	titleHolder whenChangedDo: [ :m | self changed: #title: with: { m } ].
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

{ #category : #testing }
WindowPresenter >> isDialog [
	
	^ false
]

{ #category : #api }
WindowPresenter >> isDisplayed [

	^ self window 
		ifNotNil: [ :w | w isDisplayed ]
		ifNil: [ false ] 
]

{ #category : #testing }
WindowPresenter >> isOpen [
	
	^ self isClosed not
]

{ #category : #api }
WindowPresenter >> isResizeable [

	^ isResizeable value
]

{ #category : #api }
WindowPresenter >> isResizeable: aBoolean [

	isResizeable value: aBoolean
]

{ #category : #testing }
WindowPresenter >> isWindowPresenter [

	^ true
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

	aMenuPresenter owner: self.
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

{ #category : #notifying }
WindowPresenter >> notify: aSpecNotification [
	"Receives a notification and delivers it as required"

	self withAdapterDo: [ :anAdapter |
		aSpecNotification type 
			notify: aSpecNotification 
			on: anAdapter ]
]

{ #category : #private }
WindowPresenter >> okToChange [

	self flag: #TODO. "Maybe wrong?"
	self presenter canDiscardEdits ifTrue: [ ^ true ].
	"Solicit cancel from view"
	self changed: #wantToChange.  
	^ self canDiscardEdits
]

{ #category : #api }
WindowPresenter >> openModal: aWindow [ 

	self changed: #openModal: with: { aWindow }
]

{ #category : #private }
WindowPresenter >> openWithSpecLayout: aSpec [

	self buildWithSpecLayout: aSpec.
	self changed: #open with: #().
	self updateTitle.
]

{ #category : #accessing }
WindowPresenter >> presenter [
	
	^ presenter value
]

{ #category : #accessing }
WindowPresenter >> presenter: aPresenter [

	aPresenter owner: self.
	presenter value: aPresenter
]

{ #category : #'private building' }
WindowPresenter >> rebuildWithSpecLayout: aSpec [

	self changed: #rebuildWithSpecLayout: with: { aSpec }
]

{ #category : #private }
WindowPresenter >> requestWindowClose [

	^ self presenter
		ifNotNil: [ :aPresenter | aPresenter requestWindowClose ]
		ifNil: [ true ]
]

{ #category : #api }
WindowPresenter >> statusBar [

	^ statusbarHolder value
]

{ #category : #api }
WindowPresenter >> statusBar: aStatusbarPresenter [

	aStatusbarPresenter owner: self.
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

	^ titleHolder value
]

{ #category : #private }
WindowPresenter >> title: aString [

	titleHolder value: aString
]

{ #category : #api }
WindowPresenter >> toolBar [

	^ toolBarHolder value
]

{ #category : #api }
WindowPresenter >> toolBar: aToolbarPresenter [

	aToolbarPresenter owner: self.
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

	self changed: #title: with: { self title }
]

{ #category : #'api-events' }
WindowPresenter >> whenClosedDo: aBlock [

	isClosedHolder whenChangedDo: [ :value |
		value ifTrue: [ aBlock value ] ]
]

{ #category : #'api-events' }
WindowPresenter >> whenOpenedDo: aBlock [

	isClosedHolder whenChangedDo: [ :value |
		value ifFalse: [ aBlock value ] ]
]

{ #category : #accessing }
WindowPresenter >> window [ 
	
	^ windowHolder value
]

{ #category : #accessing }
WindowPresenter >> window: aWindow [
	
	windowHolder value: aWindow
]

{ #category : #updating }
WindowPresenter >> windowIsClosing [
	
	isClosedHolder value: true.
	self presenter windowIsClosing
]

{ #category : #updating }
WindowPresenter >> windowIsOpening [
	
	isClosedHolder value: false
]
