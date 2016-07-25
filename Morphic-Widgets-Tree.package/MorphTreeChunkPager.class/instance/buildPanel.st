initailize-release
buildPanel
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