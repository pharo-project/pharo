Class {
	#name : #GLMPagerBarBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'buttonsBrick',
		'pagerModel',
		'sliderBrick'
	],
	#category : 'Glamour-Morphic-Pager-Brick'
}

{ #category : #bar-accessing }
GLMPagerBarBrick >> buttonsBrick [

	^ buttonsBrick
]

{ #category : #bar-accessing }
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

{ #category : #'bar-instance creation' }
GLMPagerBarBrick >> newButtonsBrick [

	^ GLMPagerButtonsPaneBrick new
]

{ #category : #'bar-instance creation' }
GLMPagerBarBrick >> newSliderBrick [

	^ GLMPagerScrollSlidingBrick new
]

{ #category : #bar-event-handling }
GLMPagerBarBrick >> onPagePoped [

	buttonsBrick onPagePoped.
	self recomputeScrollBar.
]

{ #category : #bar-event-handling }
GLMPagerBarBrick >> onPagePushed: addedAnnouncement [

	self buttonsBrick onPagePushed: addedAnnouncement.
	self recomputeScrollBar
]

{ #category : #bar-accessing }
GLMPagerBarBrick >> pagerModel [

	^ pagerModel
]

{ #category : #bar-accessing }
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

{ #category : #bar-accessing }
GLMPagerBarBrick >> sliderBrick [

	^ sliderBrick
]

{ #category : #bar-accessing }
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
