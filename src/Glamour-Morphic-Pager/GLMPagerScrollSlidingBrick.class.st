Class {
	#name : #GLMPagerScrollSlidingBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'startDragX',
		'isResizing',
		'startResizingWidth',
		'resizingZoneWidth',
		'isRightResizing',
		'isLeftResizing',
		'pagerModel',
		'leftPosition'
	],
	#category : #'Glamour-Morphic-Pager-Brick'
}

{ #category : #draging }
GLMPagerScrollSlidingBrick >> computeScrollValue: aNumber [
	|newValue delta|
	
	delta := self owner width - self width.
	newValue := (aNumber max: 0) min: delta.
		
	^ ((delta = 0)
		ifTrue: [ 0 ]
		ifFalse: [ newValue / delta ])
]

{ #category : #draging }
GLMPagerScrollSlidingBrick >> computeWidth [

	^ self pagerModel
		ifNil: [ 0 ]
		ifNotNil: [ :m | m circleButtonSize * m actualNumberOfVisiblePages ]
]

{ #category : #draging }
GLMPagerScrollSlidingBrick >> dragResizingLeft: anEvent [
	| left right |
	
	right := self brickBounds right.
	left := anEvent position x - self owner globalBounds left - startDragX.
	left := (left max: 0) min: (right - self pagerModel circleButtonSize).
	
	self setPosition: left@right
	
]

{ #category : #draging }
GLMPagerScrollSlidingBrick >> dragResizingRight: anEvent [
	| left right |

	left := self brickBounds left.
	right := anEvent position x - self owner globalBounds left - (startDragX - self computeWidth).
	right := (right max: (left + self pagerModel circleButtonSize)) min: self owner width.
	
	self setPosition: left@right
	
]

{ #category : #draging }
GLMPagerScrollSlidingBrick >> dragSliding: evt [
	| newLeft value|
	
	newLeft := (evt position x - self owner globalBounds left - startDragX).
	value := self computeScrollValue: newLeft.
	
	self scrollTo: value.
	self synchronizeScrollTo: value.
]

{ #category : #'event-mouse' }
GLMPagerScrollSlidingBrick >> handleMouseOver: anEvent [
	super handleMouseOver: anEvent.
		
	isResizing | (self isLeftResizingZone: anEvent) | (self isRightResizingZone: anEvent)
		ifTrue: [ self setCursorToResizing: anEvent ]
		ifFalse: [ self setCursorToNormal: anEvent ].
]

{ #category : #'event-mouse' }
GLMPagerScrollSlidingBrick >> handlesMouseOverDragging: evt [
	^ true
]

{ #category : #initialization }
GLMPagerScrollSlidingBrick >> initialize [
	super initialize.
		
	resizingZoneWidth := 7.
	isRightResizing := false.
	isLeftResizing := false.
	isResizing := false.
		
	self
		normalColor: (Color veryLightGray alpha: 0.4);
		selectedColor: (Color veryLightGray alpha: 0.55);
		enable
]

{ #category : #testing }
GLMPagerScrollSlidingBrick >> isLeftResizingZone: evt [
	
	^ (self globalBounds withWidth: resizingZoneWidth) containsPoint: evt position
	
	
]

{ #category : #testing }
GLMPagerScrollSlidingBrick >> isRightResizingZone: evt [
	|resizingRectangle top left globalBounds|
	globalBounds := self globalBounds.
	
	top := globalBounds top.
	left := globalBounds right - resizingZoneWidth.
	
	resizingRectangle := left@top corner: globalBounds corner.
	
	^ resizingRectangle containsPoint: evt position.
	
	
]

{ #category : #accessing }
GLMPagerScrollSlidingBrick >> leftPosition [

	^ leftPosition ifNil: [ leftPosition := 0 ]
]

{ #category : #accessing }
GLMPagerScrollSlidingBrick >> leftPosition: anInteger [

	leftPosition := anInteger
]

{ #category : #'event-mouse' }
GLMPagerScrollSlidingBrick >> mouseDown: evt [

	startDragX := (evt position x - self globalBounds left).
	startResizingWidth := self width.
	
	isRightResizing := self isRightResizingZone: evt.
	
	"only one type of resizing is possible"
	isRightResizing
		ifFalse: [ isLeftResizing := self isLeftResizingZone: evt ]
		ifTrue: [ isLeftResizing := false ].

	isResizing := (isLeftResizing or: isRightResizing).
		
	super mouseDown: evt.
]

{ #category : #'event-mouse' }
GLMPagerScrollSlidingBrick >> mouseLeave: evt [

	self setCursorToNormal: evt.
	super mouseLeave: evt.
]

{ #category : #'event-mouse' }
GLMPagerScrollSlidingBrick >> mouseMove: evt [

	isRightResizing
		ifTrue: [ self dragResizingRight: evt ]
		ifFalse: [
			isLeftResizing
				ifTrue: [ self dragResizingLeft: evt ]
				ifFalse: [ self dragSliding: evt ] ].
]

{ #category : #'event-mouse' }
GLMPagerScrollSlidingBrick >> mouseUp: evt [
	super mouseUp: evt.
	
	pagerModel recomputeState: isResizing not.
	
	isResizing ifTrue: [ self setWidthAfterResizing ].
	isResizing := false.
		
	(self globalBounds containsPoint: evt position)
		ifFalse: [ self setCursorToNormal: evt ]
		ifTrue: [ self setCursorToResizing: evt ] 
]

{ #category : #scrolling }
GLMPagerScrollSlidingBrick >> moveLeft: left [ "Integer - pixels"
	"moves scrollbar inside parent to the specified left distance in pixels and notifies all listeners about that"
	"Highly optimised to not update layout during moving"
	| right |
	right := left + self width.
	self changedWith: [ :b |
		b translateXTo: left.
		b leftPosition: left ].
	
	pagerModel notifyScrollerBoundsChanged: left@right
]

{ #category : #'brick-layout-events' }
GLMPagerScrollSlidingBrick >> onChildrenLayouted [
	super onChildrenLayouted.
	self translateXTo: self leftPosition
]

{ #category : #'brick-layout-events' }
GLMPagerScrollSlidingBrick >> onLayouted [
	super onLayouted.
	"'slider onLayouted' logCr."
	self translateXTo: self leftPosition
]

{ #category : #'announcement handling' }
GLMPagerScrollSlidingBrick >> onPositionChanged: announcement [
	
	self scrollToPane: pagerModel firstVisiblePageIndex smooth: announcement smooth
]

{ #category : #'announcement handling' }
GLMPagerScrollSlidingBrick >> onResized [

	self setWidthAfterResizing
]

{ #category : #accessing }
GLMPagerScrollSlidingBrick >> pagerModel [

	^ pagerModel
]

{ #category : #accessing }
GLMPagerScrollSlidingBrick >> pagerModel: anAdapter [
	
	pagerModel := anAdapter.
			
	pagerModel announcer when: GLMNumberOfVisiblePagesChanged send: #onResized to: self.
		
	pagerModel announcer when: GLMPagerScrollPositionChanged send: #onPositionChanged: to: self.
		
	self width: self computeWidth
]

{ #category : #cursor }
GLMPagerScrollSlidingBrick >> resizeCursor [

	^ Cursor resizeForEdge: #left
]

{ #category : #scrolling }
GLMPagerScrollSlidingBrick >> scrollTo: aFloat [ "Float - percentage/100"
	"scrolls to a specified float number which must be in interval [0.0, 1.0]"
	self moveLeft: (owner width - self width) * aFloat.
]

{ #category : #scrolling }
GLMPagerScrollSlidingBrick >> scrollTo: aValue smooth: aBoolean [
	"better to ignore smooth parameter,
	because animation is not neccessary"
	self scrollTo: aValue
]

{ #category : #scrolling }
GLMPagerScrollSlidingBrick >> scrollToPane: anIndex smooth: isSmooth [

	self scrollTo: (pagerModel convertIndexToValue: anIndex) smooth: isSmooth.
]

{ #category : #cursor }
GLMPagerScrollSlidingBrick >> setCursorToNormal: evt [
		
	evt hand showTemporaryCursor: nil
]

{ #category : #cursor }
GLMPagerScrollSlidingBrick >> setCursorToResizing: evt [
	
	evt hand showTemporaryCursor: self resizeCursor
]

{ #category : #draging }
GLMPagerScrollSlidingBrick >> setPosition: aPoint [
	"aPoint x - is my bounds left
	aPoint y - is my bounds right"
	"highly optimised to not update layout and do everything very clean"
	self changedWith: [ :b | b
			translateXTo: aPoint x;
			leftPosition: aPoint x;
			doNotLayoutDuring: [
				b width: aPoint y - aPoint x.
				b wrappedBounds markWidthClean ] ].
	
	self pagerModel notifyScrollerBoundsChanged: aPoint
]

{ #category : #initialization }
GLMPagerScrollSlidingBrick >> setWidthAfterResizing [
	| left right |
	
	left := pagerModel circleButtonSize * (pagerModel firstVisiblePageIndex - 1).
	right := left + self computeWidth.
	
	self setPosition: left@right
]

{ #category : #scrolling }
GLMPagerScrollSlidingBrick >> synchronizeScrollTo: aValue [

	pagerModel synchronizeScrollTo: aValue.
]
