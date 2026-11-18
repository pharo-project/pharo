"
I represent a simple tab
I have a IconicListItem which wrap my label and my icon.
I also have a morph to display when selected.
This morph can in fact be anything responding to #asMorph. Moreover it could be a block for lazy initialization.

I also have a state representing if I am selecting or not, enabled or not, etc
"
Class {
	#name : 'TabMorph',
	#superclass : 'Morph',
	#instVars : [
		'closeable',
		'label',
		'morph',
		'state',
		'fadeOut',
		'retrievingBlock',
		'closeIcon',
		'actions',
		'menu',
		'retrievingProcess',
		'retrievingSemaphore',
		'model'
	],
	#classVars : [
		'Icons'
	],
	#category : 'Morphic-Deprecated',
	#package : 'Morphic-Deprecated'
}

{ #category : 'icons' }
TabMorph class >> closeHalfOverIcon [

	^ Icons
		at: #closeHalfOverIcon
		ifAbsentPut: [ self closeHalfOverIconContents ]
]

{ #category : 'icons - contents' }
TabMorph class >> closeHalfOverIconContents [

	^ Form
		extent: 12@12
		depth: 32
		fromArray: #(16777215 16777215 16777216 869400576 1523908608 1943339008 1943339008 1523908608 869400576 16777216 16777215 16777215 16777215 232259584 1624571904 2161311744 2161311744 2161311744 2161311744 2161311744 2161311744 1624571904 232259584 16777215 16777216 1624571904 3262657845 3429841466 2161311744 2161311744 2161311744 2161311744 3429841466 3262657845 1624571904 16777216 869400576 2161311744 3429841466 4283256141 3413129529 2161311744 2161311744 3413129529 4283256141 3429841466 2161311744 869400576 1523908608 2161311744 2161311744 3413129529 4283256141 3396352313 3396352313 4283256141 3413129529 2161311744 2161311744 1523908608 1943339008 2161311744 2161311744 2161311744 3396352313 4283256141 4283256141 3396352313 2161311744 2161311744 2161311744 1943339008 1943339008 2161311744 2161311744 2161311744 3396352313 4283256141 4283256141 3396352313 2161311744 2161311744 2161311744 1943339008 1523908608 2161311744 2161311744 3413129529 4283256141 3396352313 3396352313 4283256141 3413129529 2161311744 2161311744 1523908608 869400576 2161311744 3429841466 4283256141 3413129529 2161311744 2161311744 3413129529 4283256141 3429841466 2161311744 869400576 16777216 1624571904 3262657845 3429841466 2161311744 2161311744 2161311744 2161311744 3429841466 3262657845 1624571904 16777216 16777215 232259584 1624571904 2161311744 2161311744 2161311744 2161311744 2161311744 2161311744 1624571904 232259584 16777215 16777215 16777215 16777216 869400576 1523908608 1943339008 1943339008 1523908608 869400576 16777216 16777215 16777215)
		offset: 0@0
]

{ #category : 'icons' }
TabMorph class >> closeIcon [

	^ Icons
		at: #closeIcon
		ifAbsentPut: [ self closeIconContents ]
]

{ #category : 'icons - contents' }
TabMorph class >> closeIconContents [

	^ Form
		extent: 12@12
		depth: 32
		fromArray: #(16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 2219658573 2555202893 16777216 16777215 16777215 16777216 2555202893 2219658573 16777215 16777215 16777215 16777215 2555202893 4283256141 2538359884 16777216 16777216 2538359884 4283256141 2555202893 16777215 16777215 16777215 16777215 16777216 2538359884 4283256141 2488159822 2488159822 4283256141 2538359884 16777216 16777215 16777215 16777215 16777215 16777215 16777216 2488159822 4283256141 4283256141 2488159822 16777216 16777215 16777215 16777215 16777215 16777215 16777215 16777216 2488159822 4283256141 4283256141 2488159822 16777216 16777215 16777215 16777215 16777215 16777215 16777216 2538359884 4283256141 2488159822 2488159822 4283256141 2538359884 16777216 16777215 16777215 16777215 16777215 2555202893 4283256141 2538359884 16777216 16777216 2538359884 4283256141 2555202893 16777215 16777215 16777215 16777215 2219658573 2555202893 16777216 16777215 16777215 16777216 2555202893 2219658573 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215 16777215)
		offset: 0@0
]

{ #category : 'icons' }
TabMorph class >> closeOverIcon [

	^ Icons
		at: #closeOverIcon
		ifAbsentPut: [ self closeOverIconContents ]
]

{ #category : 'icons - contents' }
TabMorph class >> closeOverIconContents [

	^ Form
		extent: 12@12
		depth: 32
		fromArray: #(16777215 16777215 33488896 1708392448 3017015296 3872653312 3872653312 3017015296 1708392448 33488896 16777215 16777215 16777215 450363392 3218341888 4292083712 4292083712 4292083712 4292083712 4292083712 4292083712 3218341888 450363392 16777215 33488896 3218341888 4289940556 4290339683 4292018176 4292083712 4292083712 4292018176 4290339683 4289940556 3218341888 33488896 1708392448 4292083712 4290339683 4294704123 4290339683 4292018176 4292018176 4290339683 4294704123 4290339683 4292083712 1708392448 3017015296 4292083712 4292018176 4290339683 4294704123 4290338912 4290338912 4294704123 4290339683 4292018176 4292083712 3017015296 3872653312 4292083712 4292083712 4292018176 4290338912 4294704123 4294704123 4290338912 4292018176 4292083712 4292083712 3872653312 3872653312 4292083712 4292083712 4292018176 4290338912 4294704123 4294704123 4290338912 4292018176 4292083712 4292083712 3872653312 3017015296 4292083712 4292018176 4290339683 4294704123 4290338912 4290338912 4294704123 4290339683 4292018176 4292083712 3017015296 1708392448 4292083712 4290339683 4294704123 4290339683 4292018176 4292018176 4290339683 4294704123 4290339683 4292083712 1708392448 33488896 3218341888 4289940556 4290339683 4292018176 4292083712 4292083712 4292018176 4290339683 4289940556 3218341888 33488896 16777215 450363392 3218341888 4292083712 4292083712 4292083712 4292083712 4292083712 4292083712 3218341888 450363392 16777215 16777215 16777215 33488896 1708392448 3017015296 3872653312 3872653312 3017015296 1708392448 33488896 16777215 16777215)
		offset: 0@0
]

{ #category : 'accessing' }
TabMorph class >> defaultHeight [
	^ 22
]

{ #category : 'class initialization' }
TabMorph class >> initialize [

	Icons := Dictionary new
]

{ #category : 'instance creation' }
TabMorph class >> label: label icon: icon morph: morph [

	^ self new
		label: label;
		icon: icon;
		morph: morph;
		yourself
]

{ #category : 'instance creation' }
TabMorph class >> label: label icon: icon morph: morph actions: aCollection [

	^ self new
		label: label;
		icon: icon;
		morph: morph;
		addActions: aCollection;
		yourself
]

{ #category : 'instance creation' }
TabMorph class >> label: label icon: icon retrievingBlock: morph [

	^ self new
		label: label;
		icon: icon;
		retrievingBlock: morph;
		yourself
]

{ #category : 'instance creation' }
TabMorph class >> label: label icon: icon retrievingBlock: morph actions: aCollection [

	^ self new
		label: label;
		icon: icon;
		retrievingBlock: morph;
		addActions: aCollection;
		yourself
]

{ #category : 'icons' }
TabMorph class >> menuIcon [

	^ self theme windowMenuForm
]

{ #category : 'private - constants' }
TabMorph >> actionOffset [

	^ 4 * self displayScaleFactor
]

{ #category : 'accessing' }
TabMorph >> actions [

	^ actions
]

{ #category : 'protocol' }
TabMorph >> actions: aCollection [

	actions := aCollection
]

{ #category : 'protocol' }
TabMorph >> addAction: anAction [

	actions add: anAction
]

{ #category : 'protocol' }
TabMorph >> addActions: aCollection [

	actions addAll: aCollection
]

{ #category : 'icons - actions' }
TabMorph >> addActionsToIcon [

	closeIcon
		on: #mouseEnter send: #mouseEnterIcon to: self;
		on: #click send: #close to: self;
		on: #mouseLeave send: #mouseLeaveIcon to: self
]

{ #category : 'private - icons' }
TabMorph >> addIcon [

	self
		addMorph: closeIcon
		fullFrame: (LayoutFrame identity
			bottomFraction: 0;
			leftFraction: 1;
			topOffset: 6 * self displayScaleFactor;
			leftOffset: self iconRightOffset negated - (12 * self displayScaleFactor);
			rightOffset: self iconRightOffset negated;
			bottomOffset: (18 * self displayScaleFactor);
			yourself)
]

{ #category : 'private' }
TabMorph >> addLabel [

	self
		addMorph: label
		fullFrame: (LayoutFrame identity
			topFraction: 1;
			leftOffset: self labelRightOffset;
			topOffset: (label height negated - self labelBottomOffset) + 2;
			yourself)
]

{ #category : 'private' }
TabMorph >> addRefreshMenuItemOn: aMenu [
	"I have a block for retrieving my contents, so I install a menu action for this"

	retrievingBlock ifNil: [ ^ self ].
	aMenu addLine.
	aMenu
		add: 'Refresh content'
		target: self
		selector: #refreshContent
]

{ #category : 'private - drawing' }
TabMorph >> backgroundColor [
	^ state backgroundColor
]

{ #category : 'meta actions' }
TabMorph >> blueButtonUp: anEvent [
	super blueButtonUp: anEvent.
	self close
]

{ #category : 'private - drawing' }
TabMorph >> borderColor [
	"I do not use #borderColor because I want a light border"
	^ self theme lightBackgroundColor
]

{ #category : 'private - drawing' }
TabMorph >> buildBorderVertices [

	^ state buildBorderVertices
]

{ #category : 'private' }
TabMorph >> buildLabel: text withIcon: icon [

	^ (TabLabelItemMorph tab: self)
		morph:	text asMorph;
		icon: (AlphaImageMorph new image: icon);
		yourself
]

{ #category : 'private - drawing' }
TabMorph >> buildVerticesFrom: borderVertices [

	^ state buildVerticesFrom: borderVertices
]

{ #category : 'protocol' }
TabMorph >> close [

	closeable ifFalse: [ ^ self ].
	self okToClose ifFalse: [ ^self ].
	self delete
]

{ #category : 'accessing' }
TabMorph >> closeable [
	^ closeable
]

{ #category : 'protocol' }
TabMorph >> closeable: anObject [

	closeable := anObject.
	anObject
		ifTrue: [ self showCloseIcon ]
		ifFalse: [ self hideCloseIcon ]
]

{ #category : 'accessing - defaults' }
TabMorph >> defaultBounds [
	^ 0@0 corner: (100@(self class defaultHeight)) * self displayScaleFactor
]

{ #category : 'private - icons' }
TabMorph >> defaultIcon [
	^ self iconNamed: #blankIcon
]

{ #category : 'protocol' }
TabMorph >> delete [

	super delete.
	morph ifNotNil: [ morph delete ].
	retrievingProcess ifNotNil: [ retrievingProcess terminate ]
]

{ #category : 'drawing' }
TabMorph >> drawOn: aCanvas [
	| borderVertices vertices |

	borderVertices := self buildBorderVertices.
	vertices := self buildVerticesFrom: borderVertices.

	aCanvas
		drawPolygon: vertices
		color: self backgroundColor
		borderWidth: 0
		borderColor: Color transparent.

	borderVertices overlappingPairsDo: [ :start :end |
		aCanvas
			line: start
			to: end
			width: 1
			color: self borderColor ]
]

{ #category : 'accessing' }
TabMorph >> enabled [

	^ state enabled
]

{ #category : 'protocol' }
TabMorph >> enabled: aBoolean [

	(state isEnabledMatchingTheState: aBoolean) ifTrue: [ ^ self ].

	state := state newStateForEnabled: aBoolean.
	label enabled: aBoolean.
	self changed
]

{ #category : 'protocol' }
TabMorph >> extraSpaceForActions [
	| space offsets |

	space := 0.
	offsets := 1.

	self hasMenu ifTrue: [
		offsets := offsets +1.
		space := space + self menuIcon width ].

	self actions do: [ :each |
		space := space + each icon width.
		offsets := offsets +1 ].

	^ space + (offsets * self actionOffset)
]

{ #category : 'events - processing' }
TabMorph >> handleMouseUp: anEvent [
	anEvent blueButtonChanged
		ifTrue: [ self blueButtonUp: anEvent ]
		ifFalse: [ super handleMouseUp: anEvent ]
]

{ #category : 'event handling' }
TabMorph >> handlesMouseDown: event [

	^ true
]

{ #category : 'protocol' }
TabMorph >> hasActions [

	^ actions notEmpty
]

{ #category : 'protocol' }
TabMorph >> hasMenu [

	^ menu isNotNil or: [ retrievingBlock isNotNil ]
]

{ #category : 'protocol' }
TabMorph >> hideCloseIcon [

	closeIcon hide
]

{ #category : 'protocol' }
TabMorph >> icon [

	^ label icon
]

{ #category : 'protocol' }
TabMorph >> icon: anIcon [

	anIcon
		ifNil: [ label icon: nil ]
		ifNotNil: [ label icon: (AlphaImageMorph new image: anIcon) ]
]

{ #category : 'private - constants' }
TabMorph >> iconRightOffset [

	^ 10 * self displayScaleFactor
]

{ #category : 'initialization' }
TabMorph >> initialize [
	super initialize.

	self changeProportionalLayout.

	state := TabEnabledState tab: self.
	closeable := true.
	label := self buildLabel: 'tab' withIcon: self defaultIcon.
	closeIcon := AlphaImageMorph withForm: self class closeIcon scaledByDisplayScaleFactor.
	actions := OrderedCollection new.
	retrievingSemaphore := Semaphore new.

	self addLabel.
	self addIcon.
	self addActionsToIcon
]

{ #category : 'accessing' }
TabMorph >> label [
	^ label
]

{ #category : 'protocol' }
TabMorph >> label: aString [

	label morph: aString asMorph
]

{ #category : 'private - constants' }
TabMorph >> labelBottomOffset [

	^ 5 * self displayScaleFactor
]

{ #category : 'private - constants' }
TabMorph >> labelRightOffset [

	^ 12 * self displayScaleFactor
]

{ #category : 'accessing' }
TabMorph >> menu [
	| newMenu |

	newMenu := MenuMorph new defaultTarget: self.
	menu ifNotNil: [ menu cull: newMenu ].

	self addRefreshMenuItemOn: newMenu.

	^ newMenu
]

{ #category : 'protocol' }
TabMorph >> menu: aMenu [

	menu := aMenu
]

{ #category : 'private - icons' }
TabMorph >> menuIcon [

	^ self class menuIcon
]

{ #category : 'accessing' }
TabMorph >> model [

	^ model
]

{ #category : 'accessing' }
TabMorph >> model: aModel [

	model := aModel
]

{ #category : 'private - tab label' }
TabMorph >> modifyClippingBounds: clippingBounds [
	| rightOffset |

	rightOffset := -10.
	closeIcon visible ifTrue: [ rightOffset := rightOffset - 14 ].

	^ clippingBounds withRight: (clippingBounds right + rightOffset) * self displayScaleFactor
]

{ #category : 'accessing' }
TabMorph >> morph [

	^ morph
]

{ #category : 'protocol' }
TabMorph >> morph: anObject [

	morph := anObject
]

{ #category : 'event handling' }
TabMorph >> mouseDown: event [
	self flag: #pharoTodo. "Add drag"
]

{ #category : 'icons - actions' }
TabMorph >> mouseEnterIcon [

	closeable ifFalse: [ ^ self ].
	fadeOut ifNotNil: [ fadeOut terminate ].
	closeIcon image: self class closeOverIcon
]

{ #category : 'icons - actions' }
TabMorph >> mouseLeaveIcon [

	closeable ifFalse: [ ^ self ].
	fadeOut ifNotNil: [ fadeOut terminate ].
	fadeOut := [
		closeIcon image: self class closeHalfOverIcon.
		(Delay forMilliseconds: 50) wait.
		closeIcon image: self class closeIcon ] fork
]

{ #category : 'event handling' }
TabMorph >> mouseUp: event [
	"Selecting several tabs with a modifier key was done by the tab bar, listening to events of the tab"

	(event commandKeyPressed or: [ event controlKeyPressed or: [ event shiftPressed ] ]) ifTrue: [ ^ self ].
	self selected: true
]

{ #category : 'protocol' }
TabMorph >> okToClose [
	model ifNil: [ ^true ].
	^model okToClose
]

{ #category : 'private' }
TabMorph >> refreshContent [

	morph := nil.
	"Trigger the update"
	self softRetrieveMorph
]

{ #category : 'protocol' }
TabMorph >> retrieveMorph: aBlock [
	"This is a hard request. The result is blocking contrary to softRetrieveMorph.
	Late binding so the morph may be built only on demand"

	morph ifNotNil: [
		aBlock value: morph.
		^ nil ].
	retrievingSemaphore isSignaled
		ifFalse: [ self softRetrieveMorph ].

	^ [ retrievingSemaphore wait.
		morph ifNotNil: [
			self morphicUIManager defer: [aBlock value: morph ]]]
				fork
]

{ #category : 'accessing' }
TabMorph >> retrievingBlock: anObject [

	retrievingBlock := anObject.
	retrievingBlock ifNotNil: [ self softRetrieveMorph ]
]

{ #category : 'accessing' }
TabMorph >> selected [

	^ state selected
]

{ #category : 'protocol' }
TabMorph >> selected: aBoolean [

	(state isSelectedMatchingTheState: aBoolean) ifTrue: [ ^ self ].

	state := state newStateForSelected: aBoolean.
	aBoolean ifTrue: [ self showCloseIcon ]

	"No need to change here since the toolbar Ibelong to will do it"
]

{ #category : 'accessing' }
TabMorph >> setActionsFrom: aBlock [

	self actions: aBlock value
]

{ #category : 'protocol' }
TabMorph >> showCloseIcon [

	closeable
		ifTrue: [ closeIcon show ]
]

{ #category : 'private - tab label' }
TabMorph >> showIconTreshold [

	^ (closeIcon visible
		ifTrue: [ 54 ]
		ifFalse: [ 40 ]) * self displayScaleFactor
]

{ #category : 'protocol' }
TabMorph >> silentlySelected: aBoolean [

	(state isSelectedMatchingTheState: aBoolean)
		ifTrue: [ ^ self ].

	state := state newStateForSelected: aBoolean.
	aBoolean
		ifTrue: [ self showCloseIcon ].

	"No need to change here since the toolbar Ibelong to will do it"
]

{ #category : 'protocol' }
TabMorph >> softRetrieveMorph [
	"Like retrieveMorph but done in a separated process"

	morph ifNotNil: [ ^ self ].
	retrievingProcess ifNotNil: [ "The retrieving process is already running, let's wait for its result"
		retrievingProcess isTerminated ifFalse: [ ^ self ] ].

	retrievingProcess := [
		                     [ morph := retrievingBlock value ] ensure: [ "I should signal all waiters here"
			                     [ retrievingSemaphore isEmpty ] whileFalse: [ retrievingSemaphore signal ] ] ] forkAt:
		                     Processor activePriority - 2
]

{ #category : 'protocol' }
TabMorph >> themeChanged [
	super themeChanged.
	morph ifNotNil: #themeChanged
]

{ #category : 'protocol' }
TabMorph >> updateShowIcon [

	(self selected not and: [ self width < 100 ])
		ifTrue: [ self hideCloseIcon ]
		ifFalse: [ self showCloseIcon ]
]

{ #category : 'protocol' }
TabMorph >> waitReadyMorphFor: timeoutDuration [
	"This method allow wait some small duration until morph will be ready.
	Clients could use it to not show loading progress if it is really fast process.
	Instead some blinking could happen"

	morph ifNotNil: [ ^ self ].
	retrievingSemaphore waitTimeoutMilliseconds:
		timeoutDuration asMilliSeconds
]
