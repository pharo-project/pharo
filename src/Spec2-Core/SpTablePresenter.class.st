"
A TablePresenter is a spec presenter for a table.
See AbstractWidgetPresenter

self example

"
Class {
	#name : #SpTablePresenter,
	#superclass : #SpAbstractListPresenter,
	#traits : 'SpTSearchable',
	#classTraits : 'SpTSearchable classTrait',
	#instVars : [
		'#columns => SpObservableSlot',
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #specs }
SpTablePresenter class >> adapterName [

	^ #TableAdapter
]

{ #category : #api }
SpTablePresenter >> addColumn: aColumn [

	columns := columns copyWith: aColumn
]

{ #category : #api }
SpTablePresenter >> addColumns: aCollection [

	aCollection do: [ :each | self addColumn: each ]
]

{ #category : #api }
SpTablePresenter >> beNotResizable [
	self isResizable: false
]

{ #category : #api }
SpTablePresenter >> beResizable [
	self isResizable: true
]

{ #category : #accessing }
SpTablePresenter >> columns [

	^ columns
]

{ #category : #api }
SpTablePresenter >> columns: aCollection [

	columns := aCollection
]

{ #category : #api }
SpTablePresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #initialization }
SpTablePresenter >> initialize [ 

	super initialize.
	self initializeTSearchable.
	showColumnHeaders := true.
	columns := #().
	isResizable := false
]

{ #category : #accessing }
SpTablePresenter >> isResizable [
	^ isResizable
]

{ #category : #accessing }
SpTablePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTablePresenter >> isShowingColumnHeaders [

	^ showColumnHeaders
]

{ #category : #private }
SpTablePresenter >> searchValueOf: anObject [
	
	^ anObject asString
]

{ #category : #api }
SpTablePresenter >> showColumnHeaders [

	showColumnHeaders := true
]

{ #category : #testing }
SpTablePresenter >> valueAtColumn: aColumn row: aRow [ 

	^ (columns at: aColumn) readObject: (self model items at: aRow)
]

{ #category : #'api-events' }
SpTablePresenter >> whenColumnsChangedDo: aBlock [
	
	self property: #columns whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTablePresenter >> whenIsResizableChangedDo: aBlock [
	self property: #isResizable whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTablePresenter >> whenShowColumnHeadersChangedDo: aBlock [
	
	self property: #showColumnHeaders whenChangedDo: aBlock
]
