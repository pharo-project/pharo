"
I'm a presenter for a tabbed notebook container.
"
Class {
	#name : #NotebookPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'pagesHolder',
		'selectedPageHolder'
	],
	#category : #'Spec-Core-Widgets-Tab'
}

{ #category : #specs }
NotebookPresenter class >> adapterName [

	^ #NotebookAdapter
]

{ #category : #accessing }
NotebookPresenter >> addPage: aPage [

	aPage owner: self.
	pagesHolder add: aPage
]

{ #category : #private }
NotebookPresenter >> basicSelectedPage: aPage [

	selectedPageHolder value: aPage
]

{ #category : #initialization }
NotebookPresenter >> initialize [

	super initialize.
	pagesHolder := OrderedCollection new asValueHolder.
	selectedPageHolder := nil asValueHolder.
	
	pagesHolder whenChangedDo: [ self pagesChanged ].
	
]

{ #category : #accessing }
NotebookPresenter >> pageAt: index [

	^ self pages at: index
]

{ #category : #private }
NotebookPresenter >> pageTitleChanged: aPage [ 

	self changed: #updatePageTitle: with: { aPage }
]

{ #category : #accessing }
NotebookPresenter >> pages [ 

	^ pagesHolder value
]

{ #category : #accessing }
NotebookPresenter >> pages: aCollection [

	aCollection do: [ :each |
		self addPage: each ]
]

{ #category : #private }
NotebookPresenter >> pagesChanged [

	self changed: #updatePages
]

{ #category : #accessing }
NotebookPresenter >> resetAllPageContents [

	self pages do: #resetContent
]

{ #category : #accessing }
NotebookPresenter >> selectedPage [ 

	^ selectedPageHolder value
]

{ #category : #accessing }
NotebookPresenter >> selectedPage: aPage [

	aPage = self selectedPage ifTrue: [ ^self ].
	self basicSelectedPage: aPage.
	self changed: #selectedPage
]

{ #category : #accessing }
NotebookPresenter >> selectedPageIndex [
	| page |
	
	page := self selectedPage.
	page ifNil: [ ^ 0 ].
	
	^ self pages indexOf: page
]

{ #category : #accessing }
NotebookPresenter >> selectedPageIndex: aNumber [

	self selectedPage: (self pages at: aNumber)
]

{ #category : #private }
NotebookPresenter >> updatePageContent: aPage [

	self changed: #updatePageContent: with: { aPage }
]

{ #category : #'api-events' }
NotebookPresenter >> whenSelectedPageChanged: aBlock [

	selectedPageHolder whenChangedDo: aBlock
]
