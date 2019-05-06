Class {
	#name : #TablePresenter,
	#superclass : #AbstractListPresenter,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#columns => SpecObservableSlot',
		'#showColumnHeaders => SpecObservableSlot',
		'#isResizableHolder'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #specs }
TablePresenter class >> adapterName [

	^ #TableAdapter
]

{ #category : #api }
TablePresenter >> addColumn: aColumn [

	columns := columns copyWith: aColumn
]

{ #category : #api }
TablePresenter >> addColumns: aCollection [

	aCollection do: [ :each | self addColumn: each ]
]

{ #category : #api }
TablePresenter >> beNotResizable [
	self isResizable: false
]

{ #category : #api }
TablePresenter >> beResizable [
	self isResizable: true
]

{ #category : #accessing }
TablePresenter >> columns [

	^ columns
]

{ #category : #api }
TablePresenter >> columns: aCollection [

	columns := aCollection
]

{ #category : #api }
TablePresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #initialization }
TablePresenter >> initialize [ 

	self class initializeSlots: self.
	super initialize.
	showColumnHeaders := true.
	columns := #().
	isResizableHolder := false asValueHolder.
]

{ #category : #accessing }
TablePresenter >> isResizable [
	^ isResizableHolder value
]

{ #category : #accessing }
TablePresenter >> isResizable: aBoolean [
	isResizableHolder value: aBoolean
]

{ #category : #testing }
TablePresenter >> isShowingColumnHeaders [

	^ showColumnHeaders
]

{ #category : #api }
TablePresenter >> showColumnHeaders [

	showColumnHeaders := true
]

{ #category : #'api-events' }
TablePresenter >> whenColumnsChangedDo: aBlock [
	
	self property: #columns whenChangedDo: aBlock
]

{ #category : #'api-events' }
TablePresenter >> whenIsResizableChangedDo: aBlock [
	isResizableHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
TablePresenter >> whenShowColumnHeadersChangedDo: aBlock [
	
	self property: #showColumnHeaders whenChangedDo: aBlock
]
