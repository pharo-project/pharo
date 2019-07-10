Extension { #name : #GLMPagerModel }

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> assignFocusableMorph: aMorph withPaneAt: paneIndex [
	"assigns previosely focused morph with corresponding pane"
	
	focusedViews at: paneIndex put: aMorph.
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> doNotNotifyDuring: aBlock [
	|allowed|
	allowed := self isNotificationsAllowed.
	isNotificationsAllowed := false.
	aBlock cull: self.
	allowed ifTrue: [ isNotificationsAllowed := true ]
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> focusPaneAt: pageIndex [
	"tries to focus previosely focused morph at specified pane by its index"
	
	( self isFocusedMorphVisibleAt: pageIndex ) ifTrue: [
		focusedPageIndex := pageIndex.
		(self focusedViewAt: pageIndex) activeHand newKeyboardFocus: (self focusedViewAt: pageIndex).
		self notifyFocusedPageChanged.
	].
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> focusedMorphChanged: aMorph at: pageIndex [
	"call this method if a morph was focused in a pane with specified index. As a result corresponding pane will be marked as selected"
	
	
	self assignFocusableMorph: aMorph withPaneAt: pageIndex.
	
	focusedPageIndex = pageIndex ifFalse: [ 
		focusedPageIndex := pageIndex.
		self notifyFocusedPageChanged ]
	
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> isFocusedMorphVisibleAt: paneIndex [
	|morph|
	
	morph := self focusedViewAt: paneIndex.
	morph ifNil: [ ^ false ].
	^ morph isInWorld.
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> popAndReplacePane: aMorph [
	
	self doNotNotifyDuring: [ :m |
		m popPane;
			pushPane: aMorph smooth: false ].
		
	self notifyPagePoppedAndReplaced: aMorph
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> popPane [

	self popPane: false.
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> popPane: isSmooth [

	self invariant.
	
	pagesCount >= 1 ifTrue: [
		|scrollTo smooth|
		smooth := isSmooth.
		
		focusedViews removeKey: pagesCount  ifAbsent: [ true ].
		pagesCount := pagesCount - 1.
		
		firstVisiblePageIndex = 1 ifTrue: [ smooth := false ].
		
		lastVisiblePageIndex > pagesCount ifTrue: [
			firstVisiblePageIndex := firstVisiblePageIndex - 1.
			lastVisiblePageIndex := lastVisiblePageIndex - 1.
		].
	
		firstVisiblePageIndex := firstVisiblePageIndex max: 1.
	
		pagesCount = 0 ifTrue: [
			selectedPageIndex := 0.
			firstVisiblePageIndex := 0.
		].
		
		self selectedPageIndex > pagesCount ifTrue: [
			self selectedPageIndex: pagesCount.
		].
	
		scrollTo := ((pagesCount = firstVisiblePageIndex) & (pagesCount > self numberOfVisiblePages)).
		self notifyPagePoped: smooth.
	].

	self invariant.
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> selectToLeft [
	"moves focus to left"
	
	self selectedPageIndex: (self selectedPageIndex - 1).
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> selectToRight [
	"moves focus to right"
	
	self selectedPageIndex: (self selectedPageIndex + 1).
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> selectedPageIndex: anIndex [
	| normalizedIndex |
	"changes current selected page and scrolls to it if necessary. Also if possible re-focused available morph"
	
	normalizedIndex := self normalizePaneIndex: anIndex.
	
	(self isPaneVisible: normalizedIndex) ifFalse: [
		"we need to scroll if new selected page is invisible"
		self scrollToPane: (normalizedIndex < self firstVisiblePageIndex ifTrue: [ normalizedIndex ] ifFalse: [(normalizedIndex - self numberOfVisiblePages + 1)]).
	].
	
	selectedPageIndex := normalizedIndex.
	self focusPaneAt: normalizedIndex.
	self notifySelectedPageChanged.

	self invariant.
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> switchPaneTo: anIndex [
	| normalizedIndex |
	"switches pane to specified. Works as scrollToPane, but additionaly selects and focuses that pane."
	
	self invariant.
	
	normalizedIndex := self normalizePaneIndex: anIndex.
	
	self scrollToPane: normalizedIndex.	
	self focusPaneAt: normalizedIndex.
	
	selectedPageIndex := normalizedIndex.
	self notifySelectedPageChanged.
	
	self invariant.
]

{ #category : #'*Glamour-Morphic-Pager' }
GLMPagerModel >> updatePreview: anIndex [

	self announcer announce: (GLMPagerPreviewUpdate new pageIndex: anIndex )
]
