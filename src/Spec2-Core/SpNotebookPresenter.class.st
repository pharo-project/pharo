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

	aCollection do: [ :each | each owner: self ].
	pagesHolder rawValue: aCollection.
	pagesHolder valueChanged: nil
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

	aPage = self selectedPage ifTrue: [ ^ self ].
	selectedPage := aPage
]

{ #category : #accessing }
SpNotebookPresenter >> selectPageIndex: aNumber [
	self selectPage: (self pages at: aNumber)
]

{ #category : #accessing }
SpNotebookPresenter >> selectedPage [

	^ selectedPage
]

{ #category : #private }
SpNotebookPresenter >> selectedPage: aPage [

	self 
		deprecated: 'Use #selectPage: instead' 
		transformWith: '`@receiver selectedPage: `@statements1'
				-> '`@receiver selectPage: `@statements1'.
				
	self selectPage: aPage
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
	self 
		deprecated: 'Use #selectPageIndex: instead' 
		transformWith: '`@receiver selectedPageIndex: `@statements1'
				-> '`@receiver selectPageIndex: `@statements1'.
				
	self selectPageIndex: aNumber
]

{ #category : #'private-traversing' }
SpNotebookPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	excludes add: self.
	aBlock value: self.
	
	self selectedPage ifNil: [ ^ self ].
	self selectedPage activePresenter
		traverseInFocusOrderDo: aBlock 
		excluding: excludes
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
