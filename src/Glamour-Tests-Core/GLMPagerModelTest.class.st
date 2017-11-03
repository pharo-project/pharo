Class {
	#name : #GLMPagerModelTest,
	#superclass : #TestCase,
	#instVars : [
		'pagerModel'
	],
	#category : #Glamour-Tests-Core
}

{ #category : #private }
GLMPagerModelTest >> createSimplePaneMorph [

	^ Morph new
]

{ #category : #running }
GLMPagerModelTest >> setUp [

	pagerModel := GLMPagerModel new scrollingDelay: 0.
	pagerModel defaultNumberOfVisiblePages: 2.
]

{ #category : #tests }
GLMPagerModelTest >> testDiminishToLeft [
	|counter left|
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel numberOfVisiblePages.
	"position of left edge is a constant while diminishing to left"
	left := pagerModel firstVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
	self assert: pagerModel firstVisiblePageIndex equals: left.
	
	"diminish until numberOfVisible pages > 1"
	[ counter > 1 ] whileTrue: [
		pagerModel diminishToLeft.
		counter := counter - 1.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
	].

	"now the number of visible pages must be not less then 1"
	(1 to: 3) do: [ :i|
		pagerModel diminishToLeft.
		self assert: pagerModel numberOfVisiblePages equals: 1.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
	].

]

{ #category : #tests }
GLMPagerModelTest >> testDiminishToRight [
	|counter right|
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel defaultNumberOfVisiblePages.
	"position of right edge is a constant while diminishing to right"
	right := pagerModel lastVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
	self assert: pagerModel lastVisiblePageIndex equals: right.
	
	"diminish until numberOfVisible pages > 1"
	[ counter > 1 ] whileTrue: [
		pagerModel diminishToRight.
		counter := counter - 1.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
	].

	"now the number of visible pages must be not less then 1"
	(1 to: 3) do: [ :i|
		pagerModel diminishToRight.
		self assert: pagerModel numberOfVisiblePages equals: 1.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
	].

]

{ #category : #tests }
GLMPagerModelTest >> testEmptyPopPane [
	| count |
	
	count := 0.
	pagerModel announcer when: GLMPagePoped do: [ :ann | count := count + 1 ].
	
	pagerModel popPane.
	
	self assert: count equals: 0.
	
	self assert: pagerModel size equals: 0.
	self assert: pagerModel firstVisiblePageIndex equals: 0.
	self assert: pagerModel lastVisiblePageIndex equals: 0.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	pagerModel popPane.
	
	self assert: count equals: 0.
	
]

{ #category : #tests }
GLMPagerModelTest >> testEnlargeToLeft [
	|counter right |
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
		
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel numberOfVisiblePages.
	"position of right edge is a constant while enlarging to left"
	right := pagerModel lastVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
	self assert: pagerModel lastVisiblePageIndex equals: right.
	
	"enlarge until numberOfVisible pages > 1"
	[ counter < 6 ] whileTrue: [
		counter := counter + 1.
		pagerModel enlargeToLeft.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
	].

	"now the number of visible pages must be not bigger then 6"
	(1 to: 3) do: [ :i|
		pagerModel enlargeToLeft.
		self assert: pagerModel numberOfVisiblePages equals: 6.
		self assert: pagerModel numberOfVisiblePages equals: right - pagerModel firstVisiblePageIndex+1.
		self assert: pagerModel lastVisiblePageIndex equals: right.
		self assert: pagerModel firstVisiblePageIndex equals: 1.
	].

]

{ #category : #tests }
GLMPagerModelTest >> testEnlargeToRight [
	|counter left|
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	pagerModel scrollTo: 0.0 smooth: false.
	
	"assert that by default after creation the number of visible pages equals default value"
	counter := pagerModel numberOfVisiblePages.
	"position of left edge is a constant while enlargingto right"
	left := pagerModel firstVisiblePageIndex.
	self assert: pagerModel numberOfVisiblePages equals: counter.
	self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
	self assert: pagerModel firstVisiblePageIndex equals: left.
	
	"diminish until numberOfVisible pages > 1"
	[ counter < 6 ] whileTrue: [
		counter := counter + 1.
		pagerModel enlargeToRight.
		self assert: pagerModel numberOfVisiblePages equals: counter.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
	].

	"now the number of visible pages must be not bigger than 6"
	(1 to: 3) do: [ :i|
		pagerModel enlargeToRight.
		self assert: pagerModel numberOfVisiblePages equals: 6.
		self assert: pagerModel numberOfVisiblePages equals: pagerModel lastVisiblePageIndex-left+1.
		self assert: pagerModel firstVisiblePageIndex equals: left.
		self assert: pagerModel lastVisiblePageIndex equals: 6.
	].

]

{ #category : #tests }
GLMPagerModelTest >> testIsPaneVisible [

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	self assert: (pagerModel isPaneVisible: 1) equals: false.
	self assert: (pagerModel isPaneVisible: 2) equals: false.
	self assert: (pagerModel isPaneVisible: 3) equals: false.
	self assert: (pagerModel isPaneVisible: 4) equals: false.
	self assert: (pagerModel isPaneVisible: 5) equals: true.
	self assert: (pagerModel isPaneVisible: 6) equals: true.
]

{ #category : #tests }
GLMPagerModelTest >> testModelInitialization [

	self assert: pagerModel size equals: 0.
	self assert: pagerModel firstVisiblePageIndex equals: 0.
	self assert: pagerModel lastVisiblePageIndex equals: 0.
	self assert: pagerModel actualNumberOfVisiblePages equals: 0.
	self assert: pagerModel numberOfVisiblePages equals: 2.
]

{ #category : #tests }
GLMPagerModelTest >> testOnePanePush [
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
]

{ #category : #tests }
GLMPagerModelTest >> testOneReplacePane [
	
	pagerModel popAndReplacePane: self createSimplePaneMorph.

	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
]

{ #category : #tests }
GLMPagerModelTest >> testPanePushAnnouncement [
	| count currentAnn paneMorph |
	
	count := 0.
	paneMorph := self createSimplePaneMorph.
	pagerModel announcer when: GLMPagePushed do: [ :ann | count := count + 1. currentAnn := ann ].
	
	pagerModel pushPane: paneMorph smooth: false.
	self assert: count equals: 1.
	self assert: currentAnn morph equals: paneMorph.
	
	paneMorph := self createSimplePaneMorph.
	pagerModel pushPane: paneMorph smooth: false.
	self assert: count equals: 2.
	self assert: currentAnn morph equals: paneMorph.
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	self assert: count equals: 4.
]

{ #category : #tests }
GLMPagerModelTest >> testPopLastPageOf5 [
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel popPane.
	
	self assert: pagerModel size equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	self assert: pagerModel numberOfVisiblePages equals: 2.
]

{ #category : #tests }
GLMPagerModelTest >> testPopOnePage [
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel popPane.
	
	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.
]

{ #category : #tests }
GLMPagerModelTest >> testPrivateRecomputeIndexesAfterScrollAt [

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	
	"raw first visible must be 1.0, last visible 1.0 and after rounding 1 and 1"
	pagerModel privateRecomputeIndexesAfterScrollAt: 1.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	
	"raw first visible must be 1.0, last visible 1.0 and after rounding 1 and 1"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
		
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
		
	"raw first visible must be 1.0, last visible 1.0 and after rounding 1 and 1"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.
	
	"raw first visible must be 4.4, last visible 5.4 and after rounding 4 and 5"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.85.
	
	self assert: pagerModel firstVisiblePageIndex equals: 4.
	self assert: pagerModel lastVisiblePageIndex equals: 5.
	
	"raw first visible must be 4.6, last visible 5.6 and after rounding 5 and 6"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.9.
	
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	"raw first visible must be 4.5, last visible 5.5 and after rounding 5 and 6"
	pagerModel privateRecomputeIndexesAfterScrollAt: 0.875.
	
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	"raw first visible must be 5.0, last visible 6.0 and after rounding 5 and 6"
	pagerModel privateRecomputeIndexesAfterScrollAt: 1.0.
	
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
]

{ #category : #tests }
GLMPagerModelTest >> testSelectToLeft [

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	pagerModel switchPaneTo: 6.
	
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	self assert: pagerModel selectedPageIndex equals: 6.
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 5.
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 4.
	self assert: pagerModel lastVisiblePageIndex equals: 5.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 3.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 2.
	self assert: pagerModel firstVisiblePageIndex equals: 2.
	self assert: pagerModel lastVisiblePageIndex equals: 3.
	
	pagerModel selectToLeft.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.

	
]

{ #category : #tests }
GLMPagerModelTest >> testSelectToRight [

	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	pagerModel scrollTo: 0.0 smooth: false.
	
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.
	
	pagerModel selectToRight.
	
	self assert: pagerModel selectedPageIndex equals: 2.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.
	
	pagerModel selectToRight.
	
	self assert: pagerModel selectedPageIndex equals: 3.
	self assert: pagerModel firstVisiblePageIndex equals: 2.
	self assert: pagerModel lastVisiblePageIndex equals: 3.
	
	pagerModel selectToRight.
	
	self assert: pagerModel selectedPageIndex equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	
	pagerModel selectToRight.
	
	self assert: pagerModel selectedPageIndex equals: 5.
	self assert: pagerModel firstVisiblePageIndex equals: 4.
	self assert: pagerModel lastVisiblePageIndex equals: 5.
	
	pagerModel selectToRight.
	
	self assert: pagerModel selectedPageIndex equals: 6.
	self assert: pagerModel firstVisiblePageIndex equals: 5.
	self assert: pagerModel lastVisiblePageIndex equals: 6.
]

{ #category : #tests }
GLMPagerModelTest >> testSelectionChangeAfterPopOfLastPage [
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	
	pagerModel selectedPageIndex: 5.
	self assert: pagerModel selectedPageIndex equals: 5.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 4.
	self assert: pagerModel size equals: 4.
	self assert: pagerModel firstVisiblePageIndex equals: 3.
	self assert: pagerModel lastVisiblePageIndex equals: 4.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 3.
	self assert: pagerModel size equals: 3.
	self assert: pagerModel firstVisiblePageIndex equals: 2.
	self assert: pagerModel lastVisiblePageIndex equals: 3.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 2.
	self assert: pagerModel size equals: 2.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 2.
	self assert: pagerModel numberOfVisiblePages equals: 2.
	
	pagerModel popPane.
	
	self assert: pagerModel selectedPageIndex equals: 1.
	self assert: pagerModel size equals: 1.
	self assert: pagerModel firstVisiblePageIndex equals: 1.
	self assert: pagerModel lastVisiblePageIndex equals: 1.
	self assert: pagerModel numberOfVisiblePages equals: 2.
]
