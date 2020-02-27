"
I'm a spec presenter defining a Window.
I define common behaviour for windows (like title, extent, position). 
I can also hold a menuBar, a toolBar and a statusBar.

Tipically you do not use this presenter directly but through a presenter you want to show. 
When you execute ==aPresenter openWithSpec==, what happens is that a SpWindowPresenter is created. Then the #initializeWindow: method is called and an user can configure its window there.

You can also access your window when your presenter is active by using ==#withWindowDo:==

!! Example 1 

SpPresenter subclass: #MyPresenter.

MyPresenter >> initializeWindow: aWindowPresenter

	aWindowPresenter
		title: 'Title Example';
		initialExtent: 800@600.

!! Example 2

| presenter window |

presenter := SpTextPresenter new
text: self testText;
yourself.
window := presenter openWithSpec.
presenter withWindowDo: [ :w | w title: 'A fancy looking title text' ].

"
Class {
	#name : #SpWindowPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#presenter => SpObservableSlot',
		'#initialPosition => SpObservableSlot',
		'#initialExtent => SpObservableSlot',
		'#isResizeable => SpObservableSlot',
		'#menu => SpObservableSlot',
		'#centered => SpObservableSlot',
		'#isClosed => SpObservableSlot',
		'#statusbar => SpObservableSlot',
		'#toolBar => SpObservableSlot',
		'#window => SpObservableSlot'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpWindowPresenter class >> adapterName [

	^ #WindowAdapter
]

{ #category : #'instance creation' }
SpWindowPresenter class >> application: anApplication [
	^ self new
		application: anApplication;
		yourself
]

{ #category : #accessing }
SpWindowPresenter class >> defaultTitle [

	^ 'Untitled window'
]

{ #category : #'instance creation' }
SpWindowPresenter class >> presenter: aPresenter [
	^ (self application: aPresenter application)
		presenter: aPresenter;
		yourself
]

{ #category : #api }
SpWindowPresenter >> aboutText [
	^ aboutText ifNil: [ aboutText := 'The about text for this window has not been set.' ]
]

{ #category : #api }
SpWindowPresenter >> aboutText: aString [
	aboutText := aString
]

{ #category : #api }
SpWindowPresenter >> aboutTitle [
	"Answer the title of my About window"

	(self presenter respondsTo: #aboutTitle) ifTrue: [ ^ self presenter aboutTitle ].
	
	^ self presenter toolName
]

{ #category : #menu }
SpWindowPresenter >> addMenuItemsToWindowMenu: aMenu [

	self presenter addMenuItemsToWindowMenu: aMenu
]

{ #category : #menu }
SpWindowPresenter >> addModelItemsToWindowMenu: aMenu [
	self presenter addModelItemsToWindowMenu: aMenu
]

{ #category : #'private building' }
SpWindowPresenter >> addPresenterIn: widget withSpecLayout: aSpec [
	
	self presenter ifNil: [ ^ self ].
	self initializeWindow.
	self 
		changed: #addPresenterIn:withSpecLayout:
		with: { widget. aSpec }
]

{ #category : #api }
SpWindowPresenter >> askOkToClose [

	^ askOkToClose
]

{ #category : #api }
SpWindowPresenter >> askOkToClose: aBoolean [

	askOkToClose := aBoolean
]

{ #category : #'private building' }
SpWindowPresenter >> basicBuildWithSpecLayout: presenterSpecLayout [
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

{ #category : #api }
SpWindowPresenter >> beep [ 
	
	self adapter beep
]

{ #category : #'private building' }
SpWindowPresenter >> buildWindowWithLayout: windowSpecLayout presenterLayout: presenterSpecLayout [

	"set adapter of the window"
	self adapter: (windowSpecLayout asSpLayout
		buildAdapterFor: self 
		bindings: SpBindings value).
	self window: self adapter widget.
	"add presenter"
	self addPresenterIn: self window withSpecLayout: presenterSpecLayout.
	"announce its result"
	self announce: (SpWidgetBuilt model: self widget: self window).
	self presenter announce: (SpWindowBuilt model: self).	
	
	^ self window
]

{ #category : #'private building' }
SpWindowPresenter >> buildWithSpecLayout: aSpecLayout [
	
	^ SpBindings 
		value: self application adapterBindings
		during: [ self basicBuildWithSpecLayout: aSpecLayout ]
]

{ #category : #api }
SpWindowPresenter >> cancelled [

	^ false
]

{ #category : #api }
SpWindowPresenter >> centered [
	centered := true
]

{ #category : #api }
SpWindowPresenter >> centeredRelativeTo: aModel [

	self changed: #centeredRelativeTo: with: { aModel }
]

{ #category : #api }
SpWindowPresenter >> close [

	"Only manage closing if the presenter is really open"
	self isOpen ifFalse: [ ^ self ].
	
	self changed: #close with: #().
	self application windowClosed: self
]

{ #category : #private }
SpWindowPresenter >> defaultInitialExtent [
	
	^ 400@300
]

{ #category : #private }
SpWindowPresenter >> delete [
	"DO NOT USE. This will destroy the window and maybe you do not want that :/
	 I'm keeping this here because of testing."

	self withAdapterDo: #delete
]

{ #category : #focus }
SpWindowPresenter >> giveFocusToNextFrom: aModel [ 

	^ self presenter giveFocusToNextFrom: aModel
]

{ #category : #testing }
SpWindowPresenter >> hasMenu [ 
	
	^ self menu notNil
]

{ #category : #testing }
SpWindowPresenter >> hasPresenter: aButtonPresenter [

	^ aButtonPresenter isNotNil and: [aButtonPresenter adapter notNil
		and: [ self adapter hasWidget: aButtonPresenter adapter ]]
]

{ #category : #testing }
SpWindowPresenter >> hasStatusBar [
	
	^ self statusBar notNil
]

{ #category : #testing }
SpWindowPresenter >> hasToolbar [
	^ self toolBar notNil and: [ self toolBar notEmpty ]
]

{ #category : #api }
SpWindowPresenter >> initialExtent [
	^ initialExtent ifNil: [ self defaultInitialExtent ]
]

{ #category : #api }
SpWindowPresenter >> initialExtent: anExtent [
	initialExtent := anExtent
]

{ #category : #api }
SpWindowPresenter >> initialPosition [

	^ initialPosition
]

{ #category : #api }
SpWindowPresenter >> initialPosition: aPoint [

	initialPosition := aPoint
]

{ #category : #initialization }
SpWindowPresenter >> initialize [
	super initialize.

	titleHolder := self class defaultTitle.
	isClosed := true.
	isResizeable := true.
	centered := false.
	askOkToClose := true.

	"presenter whenChangedDo: [ :m | self updateTitle ]."
	self property: #titleHolder whenChangedDo: [ :m | self changed: #title: with: {m} ].
	self property: #centered whenChangedDo: [ :m | self changed: #centered with: #() ]
]

{ #category : #initialization }
SpWindowPresenter >> initializeWindow [
		
	self presenter initializeWindow: self
]

{ #category : #testing }
SpWindowPresenter >> isCentered [
	^ centered
]

{ #category : #testing }
SpWindowPresenter >> isClosed [
	^ isClosed
]

{ #category : #testing }
SpWindowPresenter >> isDialog [
	
	^ false
]

{ #category : #api }
SpWindowPresenter >> isDisplayed [

	^ self window 
		ifNotNil: [ :w | w isDisplayed ]
		ifNil: [ false ] 
]

{ #category : #testing }
SpWindowPresenter >> isOpen [
	
	^ self isClosed not
]

{ #category : #api }
SpWindowPresenter >> isResizeable [
	^ isResizeable
]

{ #category : #api }
SpWindowPresenter >> isResizeable: aBoolean [
	isResizeable := aBoolean
]

{ #category : #testing }
SpWindowPresenter >> isWindowPresenter [

	^ true
]

{ #category : #api }
SpWindowPresenter >> maximize [

	self changed: #maximize with: #()
]

{ #category : #api }
SpWindowPresenter >> menu [
	^ menu
]

{ #category : #api }
SpWindowPresenter >> menu: aMenuPresenter [
	aMenuPresenter ifNotNil: [ aMenuPresenter owner: self ].
	^ menu := aMenuPresenter
]

{ #category : #api }
SpWindowPresenter >> minimize [

	self changed: #minimize with: #()
]

{ #category : #notifying }
SpWindowPresenter >> notify: aSpecNotification [
	"Receives a notification and delivers it as required"

	self withAdapterDo: [ :anAdapter |
		aSpecNotification type 
			notify: aSpecNotification 
			on: anAdapter ]
]

{ #category : #private }
SpWindowPresenter >> okToChange [

	self flag: #TODO. "Maybe wrong?"
	self presenter canDiscardEdits ifTrue: [ ^ true ].
	"Solicit cancel from view"
	self changed: #wantToChange.  
	^ self canDiscardEdits
]

{ #category : #private }
SpWindowPresenter >> openWithSpecLayout: aSpec [
	self buildWithSpecLayout: aSpec.
	self changed: #open with: #().
	self allPresenters do: [ :each | each announceDisplayed ].
	self updateTitle
]

{ #category : #accessing }
SpWindowPresenter >> presenter [
	^ presenter
]

{ #category : #accessing }
SpWindowPresenter >> presenter: aPresenter [
	aPresenter owner: self.
	presenter := aPresenter
]

{ #category : #'private building' }
SpWindowPresenter >> rebuildWithSpecLayout: aSpec [

	self changed: #rebuildWithSpecLayout: with: { aSpec }
]

{ #category : #private }
SpWindowPresenter >> requestWindowClose [

	^ self presenter
		ifNotNil: [ :aPresenter | aPresenter requestWindowClose ]
		ifNil: [ true ]
]

{ #category : #api }
SpWindowPresenter >> statusBar [
	^ statusbar
]

{ #category : #api }
SpWindowPresenter >> statusBar: aStatusbarPresenter [
	aStatusbarPresenter ifNotNil: [ aStatusbarPresenter owner: self ].
	^ statusbar := aStatusbarPresenter
]

{ #category : #api }
SpWindowPresenter >> taskbarIcon [
	^ self windowIcon
		ifNil: [
			self presenter
				ifNil: [ super taskbarIcon ]
				ifNotNil: #taskbarIcon ]
]

{ #category : #private }
SpWindowPresenter >> title [
	^ titleHolder
]

{ #category : #private }
SpWindowPresenter >> title: aString [
	titleHolder := aString
]

{ #category : #api }
SpWindowPresenter >> toolBar [
	^ toolBar
]

{ #category : #api }
SpWindowPresenter >> toolBar: aToolbarPresenter [
	aToolbarPresenter ifNotNil: [ aToolbarPresenter owner: self ].
	^ toolBar := aToolbarPresenter
]

{ #category : #'private-traversing' }
SpWindowPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	"I am not focusable (not with tab)"
	self presenter 
		traverseInFocusOrderDo: aBlock 
		excluding: excludes
]

{ #category : #'private-traversing' }
SpWindowPresenter >> traversePresentersDo: aBlock excluding: excludes [
	
	super traversePresentersDo: aBlock excluding: excludes.
	presenter traversePresentersDo: aBlock excluding: excludes
]

{ #category : #api }
SpWindowPresenter >> triggerCancelAction [
	"do nothing (ensure polymorphism with DialogWindow)"
]

{ #category : #api }
SpWindowPresenter >> triggerOkAction [
	"do nothing (ensure polymorphism with DialogWindow)"
]

{ #category : #private }
SpWindowPresenter >> updateTitle [
	"Update the window title"

	self changed: #title: with: { self title }
]

{ #category : #'api-events' }
SpWindowPresenter >> whenClosedDo: aBlock [
	
	self 
		property: #isClosed 
		whenChangedDo: [ :value | value ifTrue: [ aBlock value ] ]
]

{ #category : #'api-events' }
SpWindowPresenter >> whenOpenedDo: aBlock [

	self 
		property: #isClosed 
		whenChangedDo: [ :value | value ifFalse: [ aBlock value ] ]
]

{ #category : #'api-events' }
SpWindowPresenter >> whenResizingDo: aBlock [
	
	self announcer 
		when: WindowResizing
		do: aBlock
]

{ #category : #'api-events' }
SpWindowPresenter >> whenWillCloseDo: aBlock [
	
	self announcer 
		when: SpWindowWillClose
		do: aBlock
]

{ #category : #accessing }
SpWindowPresenter >> window [
	^ window
]

{ #category : #accessing }
SpWindowPresenter >> window: aWindow [
	window := aWindow
]

{ #category : #private }
SpWindowPresenter >> windowIcon [
	^ windowIcon
]

{ #category : #private }
SpWindowPresenter >> windowIcon: aForm [
	windowIcon := aForm
]

{ #category : #updating }
SpWindowPresenter >> windowIsClosing [
	isClosed := true.
	self presenter windowIsClosing
]

{ #category : #updating }
SpWindowPresenter >> windowIsOpening [
	isClosed := false
]
