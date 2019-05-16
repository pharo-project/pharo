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

{ #category : #removing }
NotebookPresenter >> removePage: aNotebookPresenter [
	pagesHolder remove: aNotebookPresenter
]

{ #category : #removing }
NotebookPresenter >> removePageAt: anIndex [
	pagesHolder removeAt: anIndex
]

{ #category : #accessing }
NotebookPresenter >> resetAllPageContents [

	self pages do: #resetContent
]

{ #category : #accessing }
NotebookPresenter >> selectPage: aPage [
	"Alias to make code more readable"

	self selectedPage: aPage
]

{ #category : #accessing }
NotebookPresenter >> selectPageIndex: aNumber [
	"Alias to make code more readable"

	^ self selectedPageIndex: aNumber
]

{ #category : #accessing }
NotebookPresenter >> selectedPage [ 

	^ selectedPageHolder value
]

{ #category : #private }
NotebookPresenter >> selectedPage: aPage [
	aPage = self selectedPage ifTrue: [ ^ self ].
	selectedPageHolder value: aPage
]

{ #category : #accessing }
NotebookPresenter >> selectedPageIndex [
	| page |
	
	page := self selectedPage.
	page ifNil: [ ^ 0 ].
	
	^ self pages indexOf: page
]

{ #category : #private }
NotebookPresenter >> selectedPageIndex: aNumber [

	self selectedPage: (self pages at: aNumber)
]

{ #category : #private }
NotebookPresenter >> updatePageContent: aPage [

	self changed: #updatePageContent: with: { aPage }
]

{ #category : #enumerating }
NotebookPresenter >> whenPagesChangedDo: aBlock [
	pagesHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
NotebookPresenter >> whenSelectedPageChangedDo: aBlock [

	selectedPageHolder whenChangedDo: aBlock
]
