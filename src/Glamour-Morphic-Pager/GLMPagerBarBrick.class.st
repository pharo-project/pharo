Class {
	#name : #GLMPagerBarBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'buttonsBrick',
		'pagerModel',
		'sliderBrick'
	],
	#category : #'Glamour-Morphic-Pager-Brick'
}

{ #category : #accessing }
GLMPagerBarBrick >> buttonsBrick [

	^ buttonsBrick
]

{ #category : #accessing }
GLMPagerBarBrick >> buttonsBrick: aBrick [

	buttonsBrick := aBrick.
	
	self buttonsBrick pagerModel: pagerModel.
	
	self buttonsBrick
		hShrinkWrap;
		vShrinkWrap.
	
	self addBrickBack: self buttonsBrick
]

{ #category : #initialization }
GLMPagerBarBrick >> initialize [
	super initialize.
	
	self
		useVerticalLinearLayout
]

{ #category : #'instance creation' }
GLMPagerBarBrick >> newButtonsBrick [

	^ GLMPagerButtonsPaneBrick new
]

{ #category : #'instance creation' }
GLMPagerBarBrick >> newSliderBrick [

	^ GLMPagerScrollSlidingBrick new
]

{ #category : #'event-handling' }
GLMPagerBarBrick >> onPagePoped [

	buttonsBrick onPagePoped.
	self recomputeScrollBar.
]

{ #category : #'event-handling' }
GLMPagerBarBrick >> onPagePushed: addedAnnouncement [

	self buttonsBrick onPagePushed: addedAnnouncement.
	self recomputeScrollBar
]

{ #category : #accessing }
GLMPagerBarBrick >> pagerModel [

	^ pagerModel
]

{ #category : #accessing }
GLMPagerBarBrick >> pagerModel: aModel [
	
	pagerModel := aModel.
	
	self pagerModel announcer when: GLMPagePushed send: #onPagePushed: to: self.
	self pagerModel announcer when: GLMPagePoped send: #onPagePoped to: self.
	
	self sliderBrick: self newSliderBrick.
	self buttonsBrick: self newButtonsBrick.
]

{ #category : #initialization }
GLMPagerBarBrick >> recomputeScrollBar [
	
	self sliderBrick ifNotNil: #setWidthAfterResizing
]

{ #category : #accessing }
GLMPagerBarBrick >> sliderBrick [

	^ sliderBrick
]

{ #category : #accessing }
GLMPagerBarBrick >> sliderBrick: aBrick [

	sliderBrick := aBrick.
	
	self sliderBrick
		pagerModel: self pagerModel;
		height: self pagerModel circleButtonSize;
		width: [ self pagerModel circleButtonSize * self pagerModel actualNumberOfVisiblePages ].
		
	self sliderBrick layout
		floating: #left.
		
	self addBrickBack: self sliderBrick
]
