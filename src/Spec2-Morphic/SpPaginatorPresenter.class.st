"
A presenter to show a paginator. 
Tipically, a paginator is associated to a miller list (See `SpMillerPaginatorPresenter`).
"
Class {
	#name : #SpPaginatorPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#visiblePages => SpObservableSlot',
		'#selectedPage => SpObservableSlot',
		'#dragging => SpObservableSlot',
		'#pagesHolder'
	],
	#category : #'Spec2-Morphic-Paginator'
}

{ #category : #specs }
SpPaginatorPresenter class >> adapterName [

	^ #PaginatorAdapter
]

{ #category : #documentation }
SpPaginatorPresenter class >> documentExamplesProtocol [
	
	^ #'*Spec2-Morphic-Examples'
]

{ #category : #api }
SpPaginatorPresenter >> addPage: aPresenter [
	"Add a page related to `aPresenter`."
	
	pagesHolder add: aPresenter.
	self selectPage: pagesHolder size
]

{ #category : #api }
SpPaginatorPresenter >> chainTo: aPresenter [
	"It will 'chain' this paginator with `aPresenter`. 
	 In order to work, `aPresenter` needs to implement this methods: 
		- `selectPage:` to react to a page change.
		- `visiblePages:` to react to an amount of pages displayed change.
		- `scrollByDeltaPercent:` to react to movement while displacing."
	
	self 
		whenSelectedPageChangedDo: [ :pageNumber | aPresenter selectPage: pageNumber ];
		whenVisiblePagesChangedDo: [ :aNumber | aPresenter visiblePages: aNumber ];
		whenDraggingDo: [ :deltaPercent | aPresenter scrollByDeltaPercent: deltaPercent ]
]

{ #category : #private }
SpPaginatorPresenter >> dragging: aNumber [

	dragging := aNumber
]

{ #category : #initialization }
SpPaginatorPresenter >> initialize [

	super initialize.
	pagesHolder := OrderedCollection new asValueHolder.
]

{ #category : #api }
SpPaginatorPresenter >> pages [
	"Answer a collection of all pages added to paginator."	

	^ pagesHolder value
]

{ #category : #api }
SpPaginatorPresenter >> pages: aCollection [
	"Set pages of paginator taken elements from `aCollection`."

	pagesHolder removeAll.
	aCollection do: [ :each | pagesHolder add: each ]
]

{ #category : #api }
SpPaginatorPresenter >> selectPage: aNumber [
	"Select (make the selection area be placed at) the page at index `aNumber`."

	selectedPage := aNumber
]

{ #category : #api }
SpPaginatorPresenter >> visiblePages [
	"Answer the paginator visible pages (visible pages are the pages marked as selected)."

	^ visiblePages
]

{ #category : #api }
SpPaginatorPresenter >> visiblePages: aNumber [
	"Set the amount of visible pages to be shown the selected area."

	visiblePages := aNumber
]

{ #category : #'api-events' }
SpPaginatorPresenter >> whenDraggingDo: aBlock [
	"Inform when selected area is being dragged (to change selected page).
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	self property: #dragging whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpPaginatorPresenter >> whenPageAddedDo: aBlock [
	"Inform when a page has been added.
	 `aBlock` receives one argument, the new page added."

	pagesHolder whenAddedDo: aBlock
]

{ #category : #'api-events' }
SpPaginatorPresenter >> whenPageRemovedDo: aBlock [
	"Inform when a page has been added.
	 `aBlock` receives one argument, the page removed."

	pagesHolder whenRemovedDo: aBlock
]

{ #category : #'api-events' }
SpPaginatorPresenter >> whenSelectedPageChangedDo: aBlock [
	"Inform when a page selection has been changed.
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #selectedPage whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpPaginatorPresenter >> whenVisiblePagesChangedDo: aBlock [
	"Inform when a visible pages has been changed.
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #visiblePages whenChangedDo: aBlock
]
