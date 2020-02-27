"
A WindowPresenter is a spec presenter for a Window
"
Class {
	#name : #WindowPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'model',
		'initialExtent',
		'title',
		'isClosedHolder',
		'isResizeable'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #specs }
WindowPresenter class >> adapterName [

	^ #WindowAdapter
]

{ #category : #specs }
WindowPresenter class >> defaultSpec [
	<spec>
	
	^ #(WindowAdapter
		adapt: #(model))
]

{ #category : #api }
WindowPresenter >> aboutText [
	"if my aboutText has not been set the fallback is:
	  - if I do have a model, use its' about text
	  - else use the behavior defined in super
	"

	^aboutText value
		ifNil: [self model
			ifNil: [ super aboutText ]
			ifNotNil: [:m | m aboutText ]]
		ifNotNil: [ aboutText value ]
]

{ #category : #api }
WindowPresenter >> aboutTitle [
	"Answer the title of my About window"

	(self model respondsTo: #aboutTitle)
		ifTrue: [^self model aboutTitle].
	^(self model respondsTo: #toolName)
		ifTrue: [self model toolName]
		ifFalse: [self model class name].

]

{ #category : #menu }
WindowPresenter >> addMenuItemsToWindowMenu: aMenu [

	self model addMenuItemsToWindowMenu: aMenu
]

{ #category : #private }
WindowPresenter >> addModelIn: widget withSpec: aSpec [

	self model ifNil: [ ^ self ].
	
	self changed: #addModelIn:withSpec: with: { widget . aSpec }
]

{ #category : #private }
WindowPresenter >> addModelIn: widget withSpecLayout: aSpec [

	self model ifNil: [ ^ self ].
	
	self changed: #addModelIn:withSpecLayout: with: { widget . aSpec }
]

{ #category : #menu }
WindowPresenter >> addModelItemsToWindowMenu: aMenu [

	self model addModelItemsToWindowMenu: aMenu
]

{ #category : #api }
WindowPresenter >> askOkToClose [

	^ self model askOkToClose
]

{ #category : #private }
WindowPresenter >> buildWithSpecLayout: aSpec [
	"Build the widget using the spec name provided as argument"
	| adapter widget |
	
	(self spec notNil and: [ self needRebuild not ])
		ifTrue: [ widget := self spec instance ]
		ifFalse: [
			adapter := self privateAdapterFromModel: self withSpec: self defaultSpecSelector.
			widget := adapter widget.
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

	self changed: #centered with: #()
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

	^ self model giveFocusToNextFrom: aModel
]

{ #category : #focus }
WindowPresenter >> giveFocusToPreviousFrom: aModel [ 

	^ self model giveFocusToPreviousFrom: aModel 
]

{ #category : #api }
WindowPresenter >> initialExtent [

	^ initialExtent value
		ifNil: [ self model 
			ifNil: [ self defaultInitialExtent ]
			ifNotNil: [:m | m extent
				ifNil: [ m initialExtent
					ifNil: [ self defaultInitialExtent ] ] ] ]
]

{ #category : #initialization }
WindowPresenter >> initialize [

	super initialize.

	model := nil asValueHolder.
	initialExtent := nil asValueHolder.
	title := self class title asValueHolder.
	isClosedHolder := false asValueHolder.
	isResizeable := true asValueHolder.
	
	model whenChangedDo: [:m | self updateTitle ].
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
WindowPresenter >> minimize [

	self changed: #minimize with: #()
]

{ #category : #api }
WindowPresenter >> modalRelativeTo: aWindow [

	self changed: #modalRelativeTo: with: { aWindow }
]

{ #category : #accessing }
WindowPresenter >> model [
	
	^ model value
]

{ #category : #api }
WindowPresenter >> model: aModel [

	model value: aModel
]

{ #category : #private }
WindowPresenter >> okToChange [

	^ self model okToChange
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

{ #category : #private }
WindowPresenter >> rebuildWithSpec: aSpec [

	self changed: #rebuildWithSpec: with: { aSpec }.
]

{ #category : #private }
WindowPresenter >> rebuildWithSpecLayout: aSpec [

	self changed: #rebuildWithSpecLayout: with: { aSpec }
]

{ #category : #api }
WindowPresenter >> taskbarIcon [

	^ model value 
		ifNil: [ super taskbarIcon ]
		ifNotNil: [ :m |
			m windowIcon
				ifNil: [ m taskbarIcon ] ]
]

{ #category : #private }
WindowPresenter >> title [

	^ self model
		ifNil: [ super title ]
		ifNotNil: [:m | m title ]
]

{ #category : #private }
WindowPresenter >> title: aString [

	self changed: #title: with: { aString }
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
	
	self model windowIsClosing 
]
