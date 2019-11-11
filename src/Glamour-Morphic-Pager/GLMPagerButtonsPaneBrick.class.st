Class {
	#name : #GLMPagerButtonsPaneBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'pagerModel',
		'buttons'
	],
	#category : #'Glamour-Morphic-Pager-Brick'
}

{ #category : #adding }
GLMPagerButtonsPaneBrick >> addButton: aBrick [

	self buttons add: aBrick.
	
	aBrick enable.
	aBrick index: self buttons size.
	aBrick announcer when: GLMPagerButtonClicked send: #onButtonClicked: to: self.
	aBrick announcer when: GLMPagerButtonSelected send: #onButtonSelected: to: self.
	aBrick announcer when: GLMPagerButtonDeselected send: #onButtonDeselected to: self.
	
	self theme pagerWidgetThemer defaultScrollButtonStyleFor: aBrick.
	
	aBrick height: self pagerModel circleButtonSize.
	aBrick width: self pagerModel circleButtonSize.
	
	self addBrickBack: aBrick.
	
	(self buttons size = 1) ifTrue: [ self updateButtonsFocus ].
		
	self updateButtons.
]

{ #category : #calculations }
GLMPagerButtonsPaneBrick >> buttonCenterByIndex: aNumber [
	| width |
	width := self pagerModel circleButtonSize.
	^ ( ( aNumber - 1 ) * ( width ) ) + ( width / 2 ).
]

{ #category : #accessing }
GLMPagerButtonsPaneBrick >> buttons [

	^ buttons ifNil: [ buttons := OrderedCollection new ]
]

{ #category : #accessing }
GLMPagerButtonsPaneBrick >> buttons: anObject [

	buttons := anObject
]

{ #category : #testing }
GLMPagerButtonsPaneBrick >> isScrollMustBeChecked: anIndex withLeft: leftBound andRight: rightBound [
	| buttonCenter |
	
	buttonCenter := self buttonCenterByIndex: anIndex.
	^ (leftBound <= buttonCenter ) & (rightBound > buttonCenter)
]

{ #category : #updating }
GLMPagerButtonsPaneBrick >> markButtonsInside: aBounds [
	|index boundsLeft boundsRight firstFound lastFound |	
	self flag: 'refactor me'.
	
	firstFound := false.
	lastFound := false.
	index := 0.
	
	(pagerModel size = 0) ifTrue: [ ^ self ].
	boundsLeft := aBounds x.
	boundsRight := aBounds y.
	
	self buttons do: [ :indicator |
		index := index + 1.
		(self isScrollMustBeChecked: index withLeft: boundsLeft andRight: boundsRight)
			ifTrue: [
				indicator check.
				firstFound ifFalse: [
					pagerModel virtualFirstVisiblePageIndex: index.
					firstFound := true.
				]
			]
			ifFalse: [
				"uncheck only if not focused"
				indicator isFocused ifFalse: [ indicator uncheck ].
				(firstFound & (lastFound = false)) ifTrue: [
					pagerModel virtualLastVisiblePageIndex: index - 1.
					lastFound := true.
				]
			].
	].
	lastFound ifFalse: [
		pagerModel virtualLastVisiblePageIndex: index.
		lastFound := true.
	]
	
		

]

{ #category : #'instance creation' }
GLMPagerButtonsPaneBrick >> newButton [

	^ GLMPagerScrollButtonBrick new
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onButtonClicked: ann [

	self pagerModel switchPaneTo: ann buttonModel index
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onButtonDeselected [

	GLMPagerPanePreviewMorph uniqueInstance hideFromWorld
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onButtonSelected: ann [

	"notify adapter that we need to update pane preview"
	GLMPagerPanePreviewMorph uniqueInstance showAt: (self buttons at: ann buttonModel index) globalBounds topCenter.
	self pagerModel updatePreview: ann buttonModel index
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onFocusedPageChanged [

	self
		updateButtonsFocus;
		updateButtons

]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onPagePoped [
	|button|
	
	button := self buttons removeLast.
	button announcer unsubscribe: self.
	
	self owner changed.
	self removeBrick: button.
	self updateButtons
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onPagePushed: addedAnnouncement [
	
	self addButton: self newButton
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onPageSwitched [

	self updateButtons
]

{ #category : #'event-handling' }
GLMPagerButtonsPaneBrick >> onScrollBoundsChanged: boundsChangedAnnouncement [

	self markButtonsInside: boundsChangedAnnouncement bounds.
	
]

{ #category : #accessing }
GLMPagerButtonsPaneBrick >> pagerModel [

	^ pagerModel
]

{ #category : #accessing }
GLMPagerButtonsPaneBrick >> pagerModel: aModel [
	
	pagerModel := aModel.
		
	self pagerModel announcer when: GLMPageSwitched send: #onPageSwitched to: self.
	self pagerModel announcer when: GLMPagerBoundsChanged send: #onScrollBoundsChanged: to: self.
	self pagerModel announcer when: GLMFocusedPageChanged send: #onFocusedPageChanged to: self.
		
	self updateButtons
]

{ #category : #updating }
GLMPagerButtonsPaneBrick >> updateButtons [
	"first we uncheck all buttons"
	self buttons select: #isFocused thenDo: #uncheck.
	"then check only necessary"
	self pagerModel size > 0 ifTrue: [
		self buttons from: self pagerModel firstVisiblePageIndex to: self pagerModel lastVisiblePageIndex do: #check ]
]

{ #category : #updating }
GLMPagerButtonsPaneBrick >> updateButtonsFocus [
	"unfocus all"
	self buttons do: #setNotFocused.
	"because it's possible when nothing has focus"
	self pagerModel focusedPageIndex
		ifNotNil: [ (self buttons at: self pagerModel focusedPageIndex) setFocused ]
	
]
