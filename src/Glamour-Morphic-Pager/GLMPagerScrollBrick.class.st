Class {
	#name : #GLMPagerScrollBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'band',
		'currentFocus',
		'pagerModel',
		'maxPanes',
		'focus',
		'currentScrollDelta',
		'animation'
	],
	#category : #'Glamour-Morphic-Pager-Brick'
}

{ #category : #accessing }
GLMPagerScrollBrick >> animation [

	^ animation
]

{ #category : #accessing }
GLMPagerScrollBrick >> band [

	^ band
]

{ #category : #accessing }
GLMPagerScrollBrick >> band: aBrick [

	band := aBrick.
	band
		useHorizontalLinearLayout;
		layoutMorphs: true;
		width: [ self band subbricksCount * self width / ((self maxPanes min: self band subbricksCount) max: 1) ];
		vSpaceFill.
	"band actually depends from myself,
	because each pane is proportional to my size"
	band layout widthDependency: self.
	
	self addBrickBack: band.
]

{ #category : #api }
GLMPagerScrollBrick >> configurePane: aBrick [
	|brick|
	brick := aBrick asBrick.
	brick
		layoutProperties: GLMBrickLayoutProperties new;
		"width of tab is proportional to number of visible panes and my width"
		width: [ self band width / self band subbricksCount ];
		vSpaceFill;
		marginLeft: self paneMargin;
		marginRight: self paneMargin.
		
	brick layout widthDependency: self band.
	^ brick
]

{ #category : #scrolling }
GLMPagerScrollBrick >> convertScrollValue: aFloat [
	self pagerModel actualNumberOfVisiblePages = 0 ifTrue: [ ^ 0 ].
	^ (self width - ((self width / self pagerModel actualNumberOfVisiblePages + self paneMargin) * self band subbricksCount)) * aFloat
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> focusChanged: anEvent [
	| pageIndex newFocus |
	
	newFocus := anEvent morph.
	
	currentFocus = newFocus ifFalse: [
		currentFocus := newFocus.
		pageIndex := self pageOf: newFocus.
		(pageIndex > 0) ifTrue: [
			pagerModel focusedMorphChanged: newFocus at: pageIndex ] ]
]

{ #category : #scrolling }
GLMPagerScrollBrick >> glmAnimateValue: aValue duration: anInteger [

	self glmAnimateValue: aValue duration: anInteger callback: nil.
]

{ #category : #scrolling }
GLMPagerScrollBrick >> glmAnimateValue: aValue duration: anInteger callback: aBlock [
	
	self animation ifNotNil: #terminate.
	animation := GLMAnimation new
		linear;
		duration: 200;
		from: self scrollValue;
		to: (self convertScrollValue: aValue);
		ensure: [ :b | b updatePaneBounds. aBlock brickValue: b ];
		animation: [ :b :v | b scrollValue: v ];
		animate: self
]

{ #category : #initialization }
GLMPagerScrollBrick >> initialize [
	super initialize.
	
	focus := Dictionary new.
	currentScrollDelta := 0.
	maxPanes := 2.
	self band: self newBand.
	Morph announcer weak 
		when: MorphGotFocus send: #focusChanged: to: self.
]

{ #category : #accessing }
GLMPagerScrollBrick >> lastPane [

	self subbricksCount = 0 ifTrue: [ ^ nil ].
	^ self panes last.
]

{ #category : #accessing }
GLMPagerScrollBrick >> maxPanes [
	^ maxPanes
]

{ #category : #accessing }
GLMPagerScrollBrick >> maxPanes: anInteger [
	maxPanes := anInteger
]

{ #category : #'instance creation' }
GLMPagerScrollBrick >> newBand [

	^ GLMScrollPaneBandBrick new
]

{ #category : #'brick-layout-events' }
GLMPagerScrollBrick >> onChildrenLayouted [
	super onChildrenLayouted.
	self band translateXTo: (self convertScrollValue: (self pagerModel convertIndexToValue: self pagerModel firstVisiblePageIndex))
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onPagePoped: popedAnnouncement [
	self popPane: popedAnnouncement smooth.
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onPagePopedAndReplaced: ann [

	self popAndReplace: ann brick
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onPagePreviewUpdate: updateAnnouncement [

	GLMPagerPanePreviewMorph uniqueInstance updateFor: (self paneAt: updateAnnouncement pageIndex).
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onPagePushed: pushedAnnouncement [

	"added new page to the end of pane"
	self pushPane: pushedAnnouncement morph
	
	
	
	

]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onPagesNumberChanged [

	self maxPanes: pagerModel numberOfVisiblePages.
	self doLayoutForce.
	self scrollToPane: pagerModel firstVisiblePageIndex smooth: false
	
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onPositionChanged: announcement [


	self scrollToPane: pagerModel firstVisiblePageIndex smooth: announcement smooth callback: announcement callback
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onSelectedPageChanged [
	
	self panes do: [ :each | each borderColor: Color transparent ].
	
	self theme pagerWidgetThemer selectedPaneStyleFor: (self panes at: pagerModel selectedPageIndex)
]

{ #category : #'event-handling' }
GLMPagerScrollBrick >> onSynchronizeScroll: announcement [

	"directly scrolls to value"
	self scrollTo: announcement value smooth: false
]

{ #category : #initialize }
GLMPagerScrollBrick >> outOfWorld: aWorld [

	Morph announcer unsubscribe: self.
	^ super outOfWorld: aWorld.
]

{ #category : #accessing }
GLMPagerScrollBrick >> pageOf: aSubMorph [

	"returns 0 if morph is not in pager"

	| parent page |

	aSubMorph ifNil: [ ^ 0 ].

	parent := aSubMorph.

	[ parent isNotNil & ( parent == self band ) not ]
		whileTrue: [ page := parent.
			parent := parent owner
			].

	^ parent ifNil: [ 0 ] ifNotNil: [ self panes indexOf: page ]
]

{ #category : #accessing }
GLMPagerScrollBrick >> pagerModel [

	^ pagerModel
]

{ #category : #accessing }
GLMPagerScrollBrick >> pagerModel: aModel [
	
	pagerModel := aModel.
	maxPanes := aModel numberOfVisiblePages.
	
	pagerModel announcer 
		when: GLMPagePushed send: #onPagePushed: to: self.
	
	pagerModel announcer 
		when: GLMPagePoped send: #onPagePoped: to: self.
	
	pagerModel announcer 
		when: GLMPagePopedAndReplaced send: #onPagePopedAndReplaced: to: self.
						
	pagerModel announcer 
		when: GLMPagerSynchronizeScrollAnnouncement send: #onSynchronizeScroll: to: self.
		
	pagerModel announcer  
		when: GLMPagerScrollPositionChanged send: #onPositionChanged: to: self.
		
	pagerModel announcer  
		when: GLMNumberOfVisiblePagesChanged send: #onPagesNumberChanged to: self.
		
	pagerModel announcer  
		when: GLMSelectedPageChanged send: #onSelectedPageChanged to: self.
		
	pagerModel announcer  
		when: GLMPagerPreviewUpdate send: #onPagePreviewUpdate: to: self.
]

{ #category : #accessing }
GLMPagerScrollBrick >> paneAt: anIndex [
	^ self panes at: anIndex.
]

{ #category : #accessing }
GLMPagerScrollBrick >> paneMargin [
	"please leave it 0, becasue otherwise it doesn't work"
	self flag: 'fix support of margin'.
	^ 0
]

{ #category : #accessing }
GLMPagerScrollBrick >> panes [

	^ self band submorphs
]

{ #category : #api }
GLMPagerScrollBrick >> popAndReplace: aBrick [
	|brick|

	brick := self configurePane: aBrick.
	brick wrappedBounds: self band lastSubbrick wrappedBounds.
	brick updateMorphBounds.
	
	self band doNotLayoutDuring: [ :b | b
		removeBrick: b lastSubbrick;
		addBrickBack: brick ].
	self changed
	
	
]

{ #category : #api }
GLMPagerScrollBrick >> popPane [
	
	self band doNotLayoutDuring: [ :b | b removeBrick: b lastSubbrick ].
	self doLayoutForce
]

{ #category : #api }
GLMPagerScrollBrick >> popPane: isSmooth [

	(isSmooth not or: [self lastPane isNil]) ifTrue: [ ^ self popPane ].
	
	self band doNotLayoutDuring: [ :b | b removeBrick: b lastSubbrick ].
	self
		scrollTo: (pagerModel convertIndexToValue: pagerModel firstVisiblePageIndex with: self band subbricksCount)
		smooth: isSmooth
		callback: [ self doLayoutForce ]
]

{ #category : #api }
GLMPagerScrollBrick >> pushPane: aMorph [

	self band addBrickBack: (self configurePane: aMorph)
]

{ #category : #scrolling }
GLMPagerScrollBrick >> scrollTo: aValue smooth: isSmooth [

 	self scrollTo: aValue smooth: isSmooth callback: nil
]

{ #category : #scrolling }
GLMPagerScrollBrick >> scrollTo: aValue smooth: isSmooth callback: aBlock [

 	isSmooth ifTrue: [ self glmAnimateValue: aValue duration: 300 callback: aBlock ]
				ifFalse: [ self scrollValue: (self convertScrollValue: aValue). self updatePaneBounds. aBlock value ].
]

{ #category : #scrolling }
GLMPagerScrollBrick >> scrollToPane: anIndex smooth: isSmooth [

	self scrollToPane: anIndex smooth: isSmooth callback: nil
]

{ #category : #scrolling }
GLMPagerScrollBrick >> scrollToPane: anIndex smooth: isSmooth callback: aBlock [

	self scrollTo: (pagerModel convertIndexToValue: anIndex) smooth: isSmooth callback: aBlock.
]

{ #category : #accessing }
GLMPagerScrollBrick >> scrollValue [

	^ currentScrollDelta
]

{ #category : #scrolling }
GLMPagerScrollBrick >> scrollValue: value [
	
	currentScrollDelta := value.
	
	self band doNotLayoutDuring: [ :b | b translateXTo: self scrollValue ].
	self changed
]

{ #category : #scrolling }
GLMPagerScrollBrick >> updatePaneBounds [

	self band updateMorphBounds
]
