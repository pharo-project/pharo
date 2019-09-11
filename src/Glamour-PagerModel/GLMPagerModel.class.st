Class {
	#name : #GLMPagerModel,
	#superclass : #Object,
	#instVars : [
		'numberOfVisiblePages',
		'announcer',
		'firstVisiblePageIndex',
		'pagesCount',
		'lastVisiblePageIndex',
		'selectedPageIndex',
		'focusedViews',
		'focusedPageIndex',
		'virtualFirstVisiblePageIndex',
		'virtualLastVisiblePageIndex',
		'scrollButtonIconRenderBlock',
		'isDependenciesShown',
		'scrollToRightTask',
		'isNotificationsAllowed',
		'scrollingDelay',
		'minimumPageNumberForScrollbar'
	],
	#category : #'Glamour-PagerModel-Pager'
}

{ #category : #accessing }
GLMPagerModel >> actualNumberOfVisiblePages [
	"returns actual number of visible pages based on first and last visible page indexes"

	(firstVisiblePageIndex = lastVisiblePageIndex) & (firstVisiblePageIndex = 0) ifTrue: [ ^ 0 ].
	^ lastVisiblePageIndex - firstVisiblePageIndex + 1.
]

{ #category : #accessing }
GLMPagerModel >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #accessing }
GLMPagerModel >> circleButtonSize [
	^ 22
]

{ #category : #scrolling }
GLMPagerModel >> convertIndexToValue: anIndex [
	
	^ self convertIndexToValue: anIndex with: self size
]

{ #category : #private }
GLMPagerModel >> convertIndexToValue: anIndex with: numberOfPagers [
	"private"
	numberOfPagers = self actualNumberOfVisiblePages
		ifTrue: [ ^ 0 ]
		ifFalse: [ ^ (anIndex - 1) / (numberOfPagers - self actualNumberOfVisiblePages ) ]
]

{ #category : #scrolling }
GLMPagerModel >> convertValueToIndex: aValue [
	
	^ (self size - self actualNumberOfVisiblePages) * aValue + 1.
]

{ #category : #accessing }
GLMPagerModel >> defaultNumberOfVisiblePages [
	^ 2
]

{ #category : #accessing }
GLMPagerModel >> defaultNumberOfVisiblePages: anInteger [
	"use it only when the number of pages is 0"
	self size = 0 ifFalse: [ GLMPagerException signal: 'You can change default number of visible pages only when there is no pages' ].
	numberOfVisiblePages := anInteger.
]

{ #category : #accessing }
GLMPagerModel >> dependencyButtonSize [
	^ self isDependenciesShown ifTrue: [ self circleButtonSize ] ifFalse: [ 0 ].
]

{ #category : #panes }
GLMPagerModel >> diminishToLeft [

	self actualNumberOfVisiblePages = 1 ifTrue: [ ^ self ].
	self virtualLastVisiblePageIndex: (self lastVisiblePageIndex - 1).
	self recomputeState.
]

{ #category : #panes }
GLMPagerModel >> diminishToRight [

	self actualNumberOfVisiblePages = 1 ifTrue: [ ^ self ].
	self virtualFirstVisiblePageIndex: (self firstVisiblePageIndex + 1).
	self recomputeState.
]

{ #category : #panes }
GLMPagerModel >> enlargeToLeft [

	self virtualFirstVisiblePageIndex: (self firstVisiblePageIndex - 1).
	self recomputeState.
]

{ #category : #panes }
GLMPagerModel >> enlargeToRight [

	self virtualLastVisiblePageIndex: (self lastVisiblePageIndex + 1).
	self recomputeState.
]

{ #category : #accessing }
GLMPagerModel >> firstVisiblePageIndex [
	^ firstVisiblePageIndex
]

{ #category : #accessing }
GLMPagerModel >> focusedPageIndex [
	^ focusedPageIndex
]

{ #category : #focus }
GLMPagerModel >> focusedViewAt: pageIndex [
	"returns previosely focused morph in the pane or nil if no morph was focused"
	
	^ focusedViews at: pageIndex ifAbsent: [ nil ]
]

{ #category : #initialization }
GLMPagerModel >> initialize [
	super initialize.

	pagesCount := 0.
	selectedPageIndex := 0.
	focusedViews := Dictionary new.
	isDependenciesShown := false.
		
	numberOfVisiblePages := self defaultNumberOfVisiblePages.
	
	firstVisiblePageIndex:= 0.
	lastVisiblePageIndex:= 0.
	
	self invariant.
]

{ #category : #invariant }
GLMPagerModel >> invariant [
	"If any of these exceptions rise, please notify us through moose-dev mail list. Your help is appreciated"
	
	self isDebug ifFalse: [ ^ self ].
	
	pagesCount >= 0 ifFalse: [ GLMPagerException signal: 'The number of page must be non negative' ].

	pagesCount > 1 ifTrue: [
		firstVisiblePageIndex >= 1 ifFalse: [GLMPagerException signal: 'First visible page index must be > 1 if number of pages > 0'].
		lastVisiblePageIndex >= 1 ifFalse: [GLMPagerException signal: 'Last visible page index must be > 1 if number of pages > 0'].
		lastVisiblePageIndex <= pagesCount ifFalse: [ GLMPagerException signal: 'Last visible page index must be less or equals to the number of pages' ].
		self actualNumberOfVisiblePages <= numberOfVisiblePages ifFalse: [ GLMPagerException signal: 'Actual number of visible pages must be less or equals to current number of visible pages' ].
		selectedPageIndex >= 1 ifFalse: [ GLMPagerException signal: 'Selected page index must be greater or equals then 1 if number of pages is more than 1' ].
	].

	pagesCount = 1 ifTrue: [ 
		firstVisiblePageIndex = 1 ifFalse: [GLMPagerException signal: 'First visible page index must be 1 if number of pages is 1'].
		lastVisiblePageIndex = 1 ifFalse: [GLMPagerException signal: 'Last visible page index must be 1 if number of pages is 1'].
		lastVisiblePageIndex = firstVisiblePageIndex ifFalse: [ GLMPagerException signal: 'First and Last visible page indexes must be equal if number of pages is 1' ].
		self actualNumberOfVisiblePages = 1 ifFalse: [ GLMPagerException signal: 'Actual number of visible pages must be 1 if number of pages is 1' ].
		selectedPageIndex = 1 ifFalse: [ GLMPagerException signal: 'Selected page index must be 1 if number of pages is 1' ].
	].

	pagesCount = 0 ifTrue: [ 
		firstVisiblePageIndex = 0 ifFalse: [GLMPagerException signal: 'First visible page index must be 0 if number of pages is 0'].
		lastVisiblePageIndex = 0 ifFalse: [GLMPagerException signal: 'Last visible page index must be 0 if number of pages is 0'].
		self actualNumberOfVisiblePages = 0 ifFalse: [ GLMPagerException signal: 'Actual number of visible pages must be 0 if there is no pages' ].
		selectedPageIndex = 0 ifFalse: [ GLMPagerException signal: 'Selected page index must be 0 if number of pages is 0' ].
	]
]

{ #category : #initialization }
GLMPagerModel >> isDebug [

	^ true
]

{ #category : #accessing }
GLMPagerModel >> isDependenciesShown [
	^ isDependenciesShown
]

{ #category : #accessing }
GLMPagerModel >> isDependenciesShown: aBoolean [
	isDependenciesShown := aBoolean
]

{ #category : #accessing }
GLMPagerModel >> isNotificationsAllowed [

	^ isNotificationsAllowed ifNil: [ isNotificationsAllowed := true ]
]

{ #category : #panes }
GLMPagerModel >> isPaneVisible: aNumber [

	^ (aNumber >= self firstVisiblePageIndex) & (aNumber <= self lastVisiblePageIndex)
]

{ #category : #accessing }
GLMPagerModel >> lastVisiblePageIndex [
	| lastIndex |
	
	lastIndex := self firstVisiblePageIndex + self numberOfVisiblePages - 1.
	^ lastIndex > self size
		ifTrue: [ self size ]
		ifFalse: [ lastIndex ]
]

{ #category : #accessing }
GLMPagerModel >> minimumPageNumberForScrollbar [
	^ minimumPageNumberForScrollbar ifNil: [ 1 ]
]

{ #category : #accessing }
GLMPagerModel >> minimumPageNumberForScrollbar: aNumber [
	minimumPageNumberForScrollbar := aNumber
]

{ #category : #panes }
GLMPagerModel >> normalizePaneIndex: anIndex [
	"retunes normalized pane index. For example: returns 1 if index < 1 and size if index > self size"
	
	anIndex < 1 ifTrue: [ ^ 1 ].
	anIndex > self size ifTrue: [ ^ self size ].
	^ anIndex.
]

{ #category : #notifying }
GLMPagerModel >> notifyAll: anAnnouncement [
	
	self isNotificationsAllowed ifTrue: [
		self announcer announce: anAnnouncement ]
]

{ #category : #notifying }
GLMPagerModel >> notifyFocusedPageChanged [

	self notifyAll: GLMFocusedPageChanged new.
]

{ #category : #notifying }
GLMPagerModel >> notifyNumberOfVisiblePagesChanged [

	self notifyAll: GLMNumberOfVisiblePagesChanged new.
]

{ #category : #notifying }
GLMPagerModel >> notifyPagePoped: isSmooth [

	self notifyAll: (GLMPagePoped new smooth: isSmooth).
]

{ #category : #notifying }
GLMPagerModel >> notifyPagePoppedAndReplaced: aBrick [

	self notifyAll: (GLMPagePopedAndReplaced new brick: aBrick)
]

{ #category : #notifying }
GLMPagerModel >> notifyPagePushed: aMorph scrollTo: isScroll smooth: isSmooth [
	
	self notifyAll: (GLMPagePushed new morph: aMorph).
	
	isScroll ifTrue: [
		self scrollingDelay > 0
			ifTrue: [ self scrollToRightTask execute: { isSmooth } ]
			ifFalse: [ self scrollToRight: isSmooth ] ]
]

{ #category : #notifying }
GLMPagerModel >> notifyScrollPositionChanged: isSmooth [

	self notifyScrollPositionChanged: isSmooth callback: nil
]

{ #category : #notifying }
GLMPagerModel >> notifyScrollPositionChanged: isSmooth callback: aBlock [

	self notifyAll: (GLMPagerScrollPositionChanged new smooth: isSmooth; callback: aBlock; yourself).
]

{ #category : #notifying }
GLMPagerModel >> notifyScrollerBoundsChanged: aBounds [

	self notifyAll: (GLMPagerBoundsChanged new bounds: aBounds).
]

{ #category : #notifying }
GLMPagerModel >> notifySelectedPageChanged [

	self notifyAll:  GLMSelectedPageChanged new.
]

{ #category : #notifying }
GLMPagerModel >> notifySynchronizeScrollTo: aValue [ 
	
	self notifyAll: (GLMPagerSynchronizeScrollAnnouncement new value: aValue).
]

{ #category : #accessing }
GLMPagerModel >> numberOfVisiblePages [
	^ numberOfVisiblePages
]

{ #category : #private }
GLMPagerModel >> privateRecomputeIndexesAfterScrollAt: aFloat [
	"while rounding fractional part < 0.5"
	|left right|
	
	self invariant.
	
	left := self convertValueToIndex: aFloat.
	right := left + self actualNumberOfVisiblePages - 1.
	firstVisiblePageIndex := left rounded.
	lastVisiblePageIndex := right rounded.
	
	lastVisiblePageIndex > self size ifTrue: [
		|delta|
		delta := lastVisiblePageIndex - self size.
		firstVisiblePageIndex := firstVisiblePageIndex - delta.
		lastVisiblePageIndex := lastVisiblePageIndex - delta.
	].
		
	self invariant.
	
	
]

{ #category : #panes }
GLMPagerModel >> pushPane: aMorph smooth: aBoolean [

	self invariant.
	
	pagesCount := pagesCount + 1.
	
	self size = 1
		ifTrue: [ 
			firstVisiblePageIndex := 1.
			lastVisiblePageIndex := 1.
			selectedPageIndex := 1.
		]
		ifFalse: [ 
			(self actualNumberOfVisiblePages < self numberOfVisiblePages) ifTrue: [
				lastVisiblePageIndex := lastVisiblePageIndex + 1.
			].
		].
	
	self notifyPagePushed: aMorph scrollTo: true smooth: aBoolean.
	self size = 1 ifTrue: [ self notifySelectedPageChanged ].
	
	self invariant.
]

{ #category : #private }
GLMPagerModel >> recomputeState [
	self recomputeState: true
	
]

{ #category : #private }
GLMPagerModel >> recomputeState: isSmooth [
	"recomputes current model state and if necessary notifies all listeners about changes"
	|tmpNumberOfVisiblePages notifyNumberOfVisiblePagesChanged|
	
	notifyNumberOfVisiblePagesChanged := false.
	
	self invariant.
	
	((virtualFirstVisiblePageIndex isNil) & (virtualLastVisiblePageIndex isNil)) ifTrue: [ ^ self ].
	virtualFirstVisiblePageIndex ifNil: [ virtualFirstVisiblePageIndex := self firstVisiblePageIndex ].
	virtualLastVisiblePageIndex ifNil: [ virtualLastVisiblePageIndex := self lastVisiblePageIndex ].
	
	tmpNumberOfVisiblePages := virtualLastVisiblePageIndex - virtualFirstVisiblePageIndex + 1.
	tmpNumberOfVisiblePages = numberOfVisiblePages ifFalse: [ notifyNumberOfVisiblePagesChanged := true ].
	numberOfVisiblePages := tmpNumberOfVisiblePages.
	
	firstVisiblePageIndex := virtualFirstVisiblePageIndex.
	lastVisiblePageIndex := virtualLastVisiblePageIndex.

	self invariant.
	
	virtualFirstVisiblePageIndex = nil.
	virtualLastVisiblePageIndex = nil.
	
	notifyNumberOfVisiblePagesChanged ifTrue: [ self notifyNumberOfVisiblePagesChanged ].
	self notifyScrollPositionChanged: isSmooth.
	
]

{ #category : #accessing }
GLMPagerModel >> scrollButtonIconRenderBlock [
	^ scrollButtonIconRenderBlock
]

{ #category : #accessing }
GLMPagerModel >> scrollButtonIconRenderBlock: anObject [
	scrollButtonIconRenderBlock := anObject
]

{ #category : #scrolling }
GLMPagerModel >> scrollTo: aFloat smooth: isSmooth [
	
	self scrollTo: aFloat smooth: isSmooth callback: nil
]

{ #category : #scrolling }
GLMPagerModel >> scrollTo: aFloat smooth: isSmooth callback: aBlock [
	"notifies all listeners to scroll at the specified value"
	
	self privateRecomputeIndexesAfterScrollAt: aFloat.
	self notifyScrollPositionChanged: isSmooth callback: aBlock
]

{ #category : #scrolling }
GLMPagerModel >> scrollToPane: pageIndex [
	
	self scrollToPane: pageIndex callback: nil
	
]

{ #category : #scrolling }
GLMPagerModel >> scrollToPane: pageIndex callback: aBlock [
	"scrolls to specified pane specified by its index"

	self scrollTo: (self convertIndexToValue: pageIndex) smooth: true callback: aBlock
	
]

{ #category : #scrolling }
GLMPagerModel >> scrollToRight: isSmooth [
	"notifies all listeners that it is time to scroll most right"

	self scrollTo: 1.0 smooth: isSmooth
]

{ #category : #scrolling }
GLMPagerModel >> scrollToRightTask [

	^ scrollToRightTask ifNil: [
		scrollToRightTask := GLMAsyncTask new.
		scrollToRightTask
			doInBackground: [ :isSmooth | isSmooth ];
			onCompleted: [ :isSmooth | self scrollToRight: isSmooth ];
			delay: self scrollingDelay ]
]

{ #category : #scrolling }
GLMPagerModel >> scrollingDelay [

	^ scrollingDelay ifNil: [ scrollingDelay := 100 ]
]

{ #category : #accessing }
GLMPagerModel >> scrollingDelay: anInteger [

	scrollingDelay := anInteger
]

{ #category : #accessing }
GLMPagerModel >> selectedPageIndex [
	^ selectedPageIndex
]

{ #category : #accessing }
GLMPagerModel >> size [
	^ pagesCount.
]

{ #category : #scrolling }
GLMPagerModel >> synchronizeScrollTo: aValue [
	
	self notifySynchronizeScrollTo: aValue.
]

{ #category : #private }
GLMPagerModel >> virtualFirstVisiblePageIndex [
	^ virtualFirstVisiblePageIndex
]

{ #category : #private }
GLMPagerModel >> virtualFirstVisiblePageIndex: anIndex [

	virtualFirstVisiblePageIndex := self normalizePaneIndex: anIndex.
	
]

{ #category : #private }
GLMPagerModel >> virtualLastVisiblePageIndex [

	^ virtualLastVisiblePageIndex
]

{ #category : #private }
GLMPagerModel >> virtualLastVisiblePageIndex: anIndex [

	virtualLastVisiblePageIndex := self normalizePaneIndex: anIndex.
]
