"
A chunk pager
"
Class {
	#name : #MorphTreeChunkPager,
	#superclass : #MorphTreeNavigationBar,
	#instVars : [
		'atBottom',
		'lastIndex',
		'pageSizeEditor',
		'nextPageAllowed'
	],
	#category : #'Morphic-Widgets-Tree-Pagination'
}

{ #category : #accessing }
MorphTreeChunkPager >> atBottom [
	^ atBottom ifNil: [atBottom := false]
]

{ #category : #accessing }
MorphTreeChunkPager >> atBottom: aBoolean [
	atBottom ~= aBoolean 
		ifTrue: [atBottom := aBoolean]

]

{ #category : #'initailize-release' }
MorphTreeChunkPager >> buildPanel [
	| widgets nextButton lastPageButton  searchEditor |
	self removeAllMorphs.
	pageSize
		ifNil: [^ self].
	self atBottom: (((treeMorph scrollValue y <= self verticalScrollbarFrontier)) and: [treeMorph vIsScrollable]) not.
	widgets := OrderedCollection new.
	(self nodeList size > self lastIndex and: [self atBottom])
		ifTrue: [
			pageSizeEditor := self
					textEntryLabel: 'Page size'
					get: #pageSize
					set: #chunkSizeInput:
					help: 'Change the page size or the number of pages if the input begins with "/"' translated
					class: String.
			pageSizeEditor hResizing: #rigid.
			pageSizeEditor width: (self preferedFont  widthOfString: '10000').
			widgets add: pageSizeEditor.			
			nextButton := self
						buttonLabel: self class smallToRightIcon
						actionSelector: #nextPage
						arguments: {}
						getEnabled: #notOnLastPage
						help: 'Next page'.
			widgets add: nextButton.
			lastPageButton := self
					buttonLabel: self class smallToRightEndIcon
					actionSelector: #fullList
					arguments: {}
					getEnabled: #notOnLastPage
					help: 'Last page'.
			widgets add: lastPageButton.
			widgets add: (self spacer: 10)].
	self withSearch
			ifTrue: [searchEditor := self
						textEntryLabel: ''
						get: #pageSearchText
						set: #pageSearchText:
						help: 'Enter a text correspondig to your search' translated
						class: String.
			searchEditor ghostText: 'Searched text'.
			widgets add: searchEditor.
			widgets add: (self spacer: 10)].
	widgets add: (self spacer: 1).
	widgets add: (LabelMorph contents: (self lastIndex asString, ' / ', self nodeList size asString) font: self preferedFont).
	self addAllMorphs: widgets.
	self updateContents
]

{ #category : #navigation }
MorphTreeChunkPager >> chunkSizeInput: aString [
	| input newPageSize |
	input := aString trimBoth.
	input ifEmpty: [treeMorph flash.
		^ false].
	newPageSize := Integer readFromString: input.
	newPageSize > 0 
		ifTrue: [self changePageSize: newPageSize.
			self nextPage.
			pageSizeEditor 
				ifNotNil: [pageSizeEditor takeKeyboardFocus]]
		ifFalse: [treeMorph flash.
			^ false].
	^ true
]

{ #category : #accessing }
MorphTreeChunkPager >> computedHeight [ 
	^ self nodeList size > self lastIndex 
		ifTrue: [super computedHeight ]
		ifFalse:[0]
]

{ #category : #accessing }
MorphTreeChunkPager >> computedHeightFromContents [
	| h |
	h := 0.
	self submorphsDo: [:sm | h := h max: sm height].
	^ h max: (self preferedFont height + 8)

]

{ #category : #accessing }
MorphTreeChunkPager >> currentNodelist [
	^ self nodeList copyFrom: 1 to: (self lastIndex min: self nodeList size)
]

{ #category : #navigation }
MorphTreeChunkPager >> fullList [
	self lastIndex < self nodeList size
		ifTrue: [self nextPage: self nodeList size]
]

{ #category : #initialization }
MorphTreeChunkPager >> initialize [
	super initialize.
	nextPageAllowed := true
]

{ #category : #navigation }
MorphTreeChunkPager >> lastIndex [
	^ lastIndex ifNil: [lastIndex := self pageSize]
]

{ #category : #navigation }
MorphTreeChunkPager >> lastIndex: anIndex [
	lastIndex := anIndex
]

{ #category : #navigation }
MorphTreeChunkPager >> nextPage [
	| last |
	nextPageAllowed
		ifTrue: [[nextPageAllowed := false.
			pageSizeEditor
				ifNotNil: [pageSizeEditor acceptTextInModel].
			self changed: #pageSize.
			treeMorph scroller hasSubmorphs
				ifTrue: [last := treeMorph scroller lastSubmorph].
			self
				nextPage: (self lastIndex + pageSize min: self nodeList size).
			last
				ifNotNil: [| b1 b2 |
					b2 := (treeMorph bounds: treeMorph innerBounds in: self world) bottom - treeMorph vExtraScrollRange.
					b1 := last boundsInWorld bottom.
					treeMorph scrollBy: 0 @ (b2 - b1)].
			self buildPanel]
				ensure: [nextPageAllowed := true]] 
]

{ #category : #navigation }
MorphTreeChunkPager >> nextPage: newLast [
	| addedNodeList |
	newLast > self lastIndex
		ifFalse: [ ^ self ].
	self lastIndex < self nodeList size
		ifTrue: [ 
			addedNodeList := self nodeList copyFrom: 1 to: newLast.
			self lastIndex: newLast.
			treeMorph scroller removeAllMorphs.
			treeMorph addSubmorphsFromNodeList: addedNodeList previouslyExpanded: treeMorph currentlyExpanded ]
]

{ #category : #testing }
MorphTreeChunkPager >> notOnLastPage [
	^ self onLastPage not
]

{ #category : #testing }
MorphTreeChunkPager >> onLastPage [
	^ self lastIndex = self nodeList size
]

{ #category : #'initailize-release' }
MorphTreeChunkPager >> updateContents [
	treeMorph vIsScrollable 
		ifFalse: [self atBottom: true].
	super updateContents
]

{ #category : #'user interface' }
MorphTreeChunkPager >> vScrollBarValue: scrollValue [
	| old |
	old := atBottom.
	self atBottom: scrollValue >= self verticalScrollbarFrontier.
	old ~= atBottom
		ifTrue: [ self buildPanel ]
]

{ #category : #'user interface' }
MorphTreeChunkPager >> verticalScrollbarFrontier [
	^ 0.98
]
