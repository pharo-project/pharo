"
A tree pager
"
Class {
	#name : #MorphTreePager,
	#superclass : #MorphTreeNavigationBar,
	#instVars : [
		'currentPageFirstIndex'
	],
	#category : #'Morphic-Widgets-Tree-Pagination'
}

{ #category : #accessing }
MorphTreePager >> allIntervals [
	| intervals |
	intervals := OrderedCollection new.
	1 to: self lastPage do: [:p | | start | intervals add: ((start := (((p - 1) * self pageSize) + 1)) -> (start + pageSize - 1))].
	intervals last value: self nodeList size.
	^ intervals
]

{ #category : #navigation }
MorphTreePager >> buildPanel [
	| widgets firstPageButton previousButton wid nextButton lastPageButton pageSizeEditor searchEditor |
	self removeAllMorphs.
	pageSize
		ifNil: [^ self].
	self nodeList
		ifNil: [^ self].
	widgets := OrderedCollection new.
	(self nodeList size > pageSize)
		ifTrue: [firstPageButton := self
						buttonLabel: self class smallToLeftEndIcon 
						actionSelector: #currentPage:
						arguments: {1}
						getEnabled: #notOnFirstPage
						help: 'First page'.
			widgets add: firstPageButton.
			previousButton := self
						buttonLabel: self class smallToLeftIcon
						actionSelector: #previousPage
						arguments: {}
						getEnabled: #notOnFirstPage
						help: 'Previous page'.
			widgets add: previousButton.
			wid := self textEntryLabel: '' get: #currentPage set: #currentPage: help: 'Index of page to view' translated class: Integer.
			wid hResizing: #rigid.
			wid width: (self preferedFont widthOfString: '1000').
			widgets add: wid.
			widgets add: (self buttonLabel: self class smallDiezeIcon actionSelector: #choosePage arguments: {} getEnabled: nil help: 'Choose page').
			nextButton := self
						buttonLabel: self class smallToRightIcon
						actionSelector: #nextPage
						arguments: {}
						getEnabled: #notOnLastPage
						help: 'Next page'.
			widgets add: nextButton.
			lastPageButton := self
						buttonLabel: self class smallToRightEndIcon
						actionSelector: #currentPage:
						arguments: {self lastPage}
						getEnabled: #notOnLastPage
						help: 'Last page'.
			widgets add: lastPageButton.
			widgets add: (self spacer: 10)].
	widgets add: (LabelMorph contents: 'Page size: ' font: self preferedFont).
	pageSizeEditor := self
				textEntryLabel: 'Page size'
				get: #pageSize
				set: #pageSizeInput:
				help: 'Change the page size or the number of pages if the input begins with "/"' translated
				class: String.
	pageSizeEditor hResizing: #rigid.
	pageSizeEditor width: (self preferedFont widthOfString: '10000').
	widgets add: pageSizeEditor.
	self withSearch
		ifTrue: [searchEditor := self
						textEntryLabel: ''
						get: #pageSearchText
						set: #pageSearchText:
						help: 'Enter a text correspondig to your search' translated
						class: String.
			searchEditor ghostText: 'Searched text'.
			widgets add: searchEditor].
	self addAllMorphs: widgets.
	self updateContents
]

{ #category : #accessing }
MorphTreePager >> choosePage [
	| choiceList chosen |
	choiceList := OrderedCollection new.
	self allIntervals 
		doWithIndex: [:assoc :idx | | choiceString |
			choiceString := idx asString, ': ', (self nodeList at: assoc key) asString, ' ... ', (self nodeList at: assoc value) asString.
			choiceList add: idx -> choiceString].
	chosen := 
		UIManager default 
			chooseFrom: (choiceList collect: [:c | c value])
			values: (choiceList collect: [:c | c key])
			lines: nil 
			title: 'Choose a page'.
	chosen ifNil: [^self].
	self currentPage: chosen.
]

{ #category : #private }
MorphTreePager >> currentNodelist [
	^ self nodeList copyFrom: self currentPageFirstIndex to: self currentPageLastIndex
]

{ #category : #accessing }
MorphTreePager >> currentPage [
	^ self currentPageLastIndex \\ pageSize > 0
		ifTrue: [(self currentPageLastIndex / pageSize) asInteger + 1]
		ifFalse: [(self currentPageLastIndex / pageSize) asInteger]
]

{ #category : #accessing }
MorphTreePager >> currentPage: anIndex [
	anIndex = self currentPage ifTrue: [^ self].
	anIndex = 1 
		ifTrue: [self setPageInterval: (1 to: (pageSize min: self nodeList size))]
		ifFalse: [ | lastIdx  |
			lastIdx := (anIndex * pageSize).
			lastIdx <= self nodeList size 
				ifTrue: [self setPageInterval: (((lastIdx - pageSize + 1) max: 1) to: (lastIdx))]
				ifFalse: [self setPageInterval: ((((self lastPage - 1) * pageSize) + 1) to: self nodeList size)]]

]

{ #category : #accessing }
MorphTreePager >> currentPageFirstIndex [
	^ currentPageFirstIndex ifNil: [currentPageFirstIndex := 1]
]

{ #category : #accessing }
MorphTreePager >> currentPageLastIndex [
	^ pageSize ifNil: [self nodeList size] ifNotNil: [(self currentPageFirstIndex + pageSize - 1) min: self nodeList size]
]

{ #category : #testing }
MorphTreePager >> hasSeveralPages [
	^ self lastPage > 1
]

{ #category : #accessing }
MorphTreePager >> lastPage [
	| p |
	p := (self nodeList size / pageSize) asInteger.
	self nodeList size \\ pageSize > 0 ifTrue: [p := p + 1].
	^ p.

]

{ #category : #accessing }
MorphTreePager >> nextPage [
	self currentPageLastIndex < self nodeList size
		ifTrue: [currentPageFirstIndex := (currentPageFirstIndex + pageSize) min: self nodeList size.
			self showCurrentPage]
]

{ #category : #accessing }
MorphTreePager >> nextPage: theIndexOfAnElement [
	"To make sure we can go to the page of an element."

	self currentPage: (self pageOfNodeIndex: theIndexOfAnElement)
]

{ #category : #testing }
MorphTreePager >> notOnFirstPage [
	^ self onFirstPage not

]

{ #category : #testing }
MorphTreePager >> notOnLastPage [
	^ self onLastPage not
]

{ #category : #testing }
MorphTreePager >> onFirstPage [
	^ self currentPageFirstIndex = 1
]

{ #category : #testing }
MorphTreePager >> onLastPage [
	^ self currentPageLastIndex = self nodeList size
]

{ #category : #accessing }
MorphTreePager >> previousPage [
	self currentPageFirstIndex > 1
		ifTrue: [currentPageFirstIndex := (currentPageFirstIndex - pageSize) max: 1.
			self showCurrentPage]
]

{ #category : #accessing }
MorphTreePager >> setPageInterval: anInterval [
	currentPageFirstIndex := anInterval first.
	self showCurrentPage.
	self changed: #currentPage
]

{ #category : #accessing }
MorphTreePager >> showCurrentPage [
	self buildPanel.
	treeMorph scroller removeAllMorphs.
	(self nodeList isNil or: [self nodeList isEmpty]) 
		ifTrue: [^ treeMorph emptySelection].
	treeMorph addSubmorphsFromNodeList.
	treeMorph updateColumnMorphs.
	treeMorph scrollSelectionIntoView 

]

{ #category : #'user interface' }
MorphTreePager >> updateForNewPageSize: newPageSize [
	pageSize ~= newPageSize
		ifTrue: [ |  oldLast |
			oldLast := self currentPageLastIndex.
			pageSize := newPageSize max: 1.
			currentPageFirstIndex := (oldLast + 1 - pageSize) max: 1.
			self nodeList ifNotNil: [self showCurrentPage]]

]
