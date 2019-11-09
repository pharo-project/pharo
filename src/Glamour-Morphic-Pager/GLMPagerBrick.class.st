Class {
	#name : #GLMPagerBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'firstCombination',
		'pagerBar',
		'scrollBrick',
		'pagerModel',
		'showScrollBarWhenNeeded'
	],
	#category : #'Glamour-Morphic-Pager-Brick'
}

{ #category : #examples }
GLMPagerBrick class >> exampleBasic [
	|pager adapter|
	adapter := GLMPagerModel new.
	pager :=	GLMPagerBrick new.
	pager pagerModel: adapter.
	pager pushPane: (GLMLabelBrick new color: Color lightRed; text: 'Hello'; textPosition: #center).
	"pager pushPane: (GLMBrick new color: Color green).
	pager pushPane: (GLMBrick new color: Color yellow).
	pager pushPane: (GLMBrick new color: Color blue).
	pager pushPane: (GLMBrick new color: Color pink)."
	
	pager height: 600.
	pager width: 1024.
	pager asBrickedMorph openInWindow.
	
	pager inspect.
]

{ #category : #initialization }
GLMPagerBrick >> initialize [
	super initialize.
	
	self useVerticalLinearLayout
]

{ #category : #initialization }
GLMPagerBrick >> initializeShortcuts [

	self
		bindKeyCombination: Character arrowLeft alt command
		toAction: [ self pagerModel selectToLeft ].
	
	self
		bindKeyCombination: Character arrowRight alt command
		toAction: [ self pagerModel selectToRight ].
	
	self
		bindKeyCombination: Character arrowLeft alt shift command
		toAction: [ self pressedScrollerSizeCombination: #left ].

	self
		bindKeyCombination: Character arrowRight alt shift command
		toAction: [ self pressedScrollerSizeCombination: #right ].
]

{ #category : #'instance creation' }
GLMPagerBrick >> newPagerBar [

	^ GLMPagerBarBrick new
]

{ #category : #'instance creation' }
GLMPagerBrick >> newScrollBrick [

	^ GLMPagerScrollBrick new
]

{ #category : #'event-handling' }
GLMPagerBrick >> onPagePoped [

	self showOrHideScrollBar
]

{ #category : #'event-handling' }
GLMPagerBrick >> onPagePushed [

	self showOrHideScrollBar
]

{ #category : #initialization }
GLMPagerBrick >> outOfWorld: aWorld [

	GLMPagerPanePreviewMorph remove.
	^ super outOfWorld: aWorld.
]

{ #category : #accessing }
GLMPagerBrick >> pagerBar [

	^ pagerBar
]

{ #category : #accessing }
GLMPagerBrick >> pagerBar: aBrick [

	pagerBar := aBrick.
	
	self pagerBar
		vShrinkWrap;
		hShrinkWrap.
	self pagerBar layout hAlign: #center.
	
	self pagerBar pagerModel: pagerModel.
	
	self addBrickBack: self pagerBar
]

{ #category : #accessing }
GLMPagerBrick >> pagerModel [
	^ pagerModel
]

{ #category : #accessing }
GLMPagerBrick >> pagerModel: aModel [

	pagerModel := aModel.
	
	self pagerModel announcer when: GLMPagePushed send: #onPagePushed to: self.
	self pagerModel announcer when: GLMPagePoped send: #onPagePoped to: self.
	
	self initializeShortcuts.
	
	self scrollBrick: self newScrollBrick.
	self pagerBar: self newPagerBar
]

{ #category : #api }
GLMPagerBrick >> popAndReplacePane: aMorph [
	"replaces last pane with new pane"
	self pagerModel popAndReplacePane: aMorph
]

{ #category : #api }
GLMPagerBrick >> popPane [
	"pops the last pane with smooth animation"
	self pagerModel popPane: true.
]

{ #category : #actions }
GLMPagerBrick >> pressedScrollerSizeCombination: aSymbol [

	firstCombination ifNil: [ firstCombination := aSymbol. ^ self].
	
	self scrollerSizeShortcutAction: firstCombination second: aSymbol.
	firstCombination := nil.
]

{ #category : #api }
GLMPagerBrick >> pushPane: aMorph [
	"pushes a pane to the end with smooth scroll animation"
	| isSmooth |
	
	isSmooth := self pagerModel size >= self pagerModel numberOfVisiblePages.
	self pagerModel pushPane: aMorph smooth: isSmooth
]

{ #category : #accessing }
GLMPagerBrick >> scrollBrick [

	^ scrollBrick
]

{ #category : #accessing }
GLMPagerBrick >> scrollBrick: aBrick [

	scrollBrick := aBrick.
	self scrollBrick pagerModel: pagerModel.
	
	self scrollBrick
		vSpaceFill;
		hSpaceFill.
	
	self addBrickBack: self scrollBrick
]

{ #category : #actions }
GLMPagerBrick >> scrollerSizeShortcutAction: first second: second [
	
	(first = #left) & (second = #left) ifTrue: [ pagerModel enlargeToLeft ].
	(first = #left) & (second = #right) ifTrue: [ pagerModel diminishToRight ].
	(first = #right) & (second = #left) ifTrue: [ pagerModel diminishToLeft ].
	(first = #right) & (second = #right) ifTrue: [ pagerModel enlargeToRight ]
]

{ #category : #actions }
GLMPagerBrick >> showOrHideScrollBar [
	
	((self pagerModel size > self pagerModel minimumPageNumberForScrollbar) & (self showScrollBarWhenNeeded))
		ifTrue: [ (self hasSubbrick: self pagerBar)
			ifFalse: [ self addBrickBack: self pagerBar ] ]
		ifFalse: [ self removeBrick: self pagerBar ].
]

{ #category : #accessing }
GLMPagerBrick >> showScrollBarWhenNeeded [
	"retuns true if scrollbar should be visible if number of pages > 1,
	otherwise false"
	^ showScrollBarWhenNeeded ifNil: [ showScrollBarWhenNeeded := true ]
]

{ #category : #accessing }
GLMPagerBrick >> showScrollBarWhenNeeded: aBoolean [

	showScrollBarWhenNeeded := aBoolean
]
