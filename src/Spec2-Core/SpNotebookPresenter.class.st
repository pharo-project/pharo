"
I'm a presenter for a tabbed notebook container.
"
Class {
	#name : #SpNotebookPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#pagesHolder',
		'#selectedPage => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Tab'
}

{ #category : #specs }
SpNotebookPresenter class >> adapterName [

	^ #NotebookAdapter
]

{ #category : #accessing }
SpNotebookPresenter >> addPage: aPage [

	aPage owner: self.
	pagesHolder add: aPage
]

{ #category : #initialization }
SpNotebookPresenter >> initialize [

	super initialize.
	pagesHolder := OrderedCollection new asValueHolder.
	selectedPage := nil.
	
	pagesHolder whenChangedDo: [ self pagesChanged ]
]

{ #category : #accessing }
SpNotebookPresenter >> pageAt: index [

	^ self pages at: index
]

{ #category : #private }
SpNotebookPresenter >> pageTitleChanged: aPage [ 

	self changed: #updatePageTitle: with: { aPage }
]

{ #category : #accessing }
SpNotebookPresenter >> pages [ 

	^ pagesHolder value
]

{ #category : #accessing }
SpNotebookPresenter >> pages: aCollection [

	aCollection do: [ :each |
		self addPage: each ]
]

{ #category : #private }
SpNotebookPresenter >> pagesChanged [

	self changed: #updatePages
]

{ #category : #removing }
SpNotebookPresenter >> removeAll [

	pagesHolder removeAll
]

{ #category : #removing }
SpNotebookPresenter >> removePage: aPage [

	pagesHolder remove: aPage
]

{ #category : #removing }
SpNotebookPresenter >> removePageAt: anIndex [

	pagesHolder removeAt: anIndex
]

{ #category : #accessing }
SpNotebookPresenter >> resetAllPageContents [

	self pages do: #resetContent
]

{ #category : #accessing }
SpNotebookPresenter >> selectPage: aPage [
	"Alias to make code more readable"

	self selectedPage: aPage
]

{ #category : #accessing }
SpNotebookPresenter >> selectPageIndex: aNumber [
	"Alias to make code more readable"

	^ self selectedPageIndex: aNumber
]

{ #category : #accessing }
SpNotebookPresenter >> selectedPage [ 

	^ selectedPage
]

{ #category : #private }
SpNotebookPresenter >> selectedPage: aPage [

	aPage = self selectedPage ifTrue: [ ^ self ].
	selectedPage := aPage
]

{ #category : #accessing }
SpNotebookPresenter >> selectedPageIndex [
	| page |
	
	page := self selectedPage.
	page ifNil: [ ^ 0 ].
	
	^ self pages indexOf: page
]

{ #category : #private }
SpNotebookPresenter >> selectedPageIndex: aNumber [

	self selectedPage: (self pages at: aNumber)
]

{ #category : #private }
SpNotebookPresenter >> updatePageContent: aPage [

	self changed: #updatePageContent: with: { aPage }
]

{ #category : #enumerating }
SpNotebookPresenter >> whenPagesChangedDo: aBlock [

	pagesHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpNotebookPresenter >> whenSelectedPageChangedDo: aBlock [

	self 
		property: #selectedPage 
		whenChangedDo: aBlock
]
