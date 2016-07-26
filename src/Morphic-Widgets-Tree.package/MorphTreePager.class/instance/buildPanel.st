navigation
buildPanel
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