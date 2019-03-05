Class {
	#name : #ColumnedListPresenter,
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
ColumnedListPresenter class >> adapterName [

	^ #ColumnedListAdapter
]

{ #category : #specs }
ColumnedListPresenter class >> example [
	<sampleInstance>
	| example |
	example := self new.
	example
		addColumn: (StringTableColumn title: 'Number' evaluated: #yourself);
		addColumn: (StringTableColumn title: 'Hex' evaluated: #hex);
		showColumnHeaders;
		items: (1 to: 10) asArray;
		openWithSpec.
	^ example
]

{ #category : #specs }
ColumnedListPresenter class >> exampleWithoutColumnHeaders [
	<sampleInstance>
	| example |
	example := self new.
	example
		addColumn: (StringTableColumn title: 'Number' evaluated: #yourself);
		addColumn: (StringTableColumn title: 'Hex' evaluated: #hex);
		items: (1 to: 10) asArray;
		hideColumnHeaders;
		openWithSpec.
	^ example
]

{ #category : #api }
ColumnedListPresenter >> addColumn: aColumn [

	columns := columns copyWith: aColumn
]

{ #category : #api }
ColumnedListPresenter >> beNotResizable [
	self isResizable: false
]

{ #category : #api }
ColumnedListPresenter >> beResizable [
	self isResizable: true
]

{ #category : #accessing }
ColumnedListPresenter >> columns [

	^ columns
]

{ #category : #api }
ColumnedListPresenter >> columns: aCollection [

	columns := aCollection
]

{ #category : #api }
ColumnedListPresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #initialization }
ColumnedListPresenter >> initialize [ 

	self class initializeSlots: self.
	super initialize.
	showColumnHeaders := true.
	columns := #().
	isResizableHolder := false asValueHolder.
]

{ #category : #accessing }
ColumnedListPresenter >> isResizable [
	^ isResizableHolder value
]

{ #category : #accessing }
ColumnedListPresenter >> isResizable: aBoolean [
	isResizableHolder value: aBoolean
]

{ #category : #testing }
ColumnedListPresenter >> isShowingColumnHeaders [

	^ showColumnHeaders
]

{ #category : #api }
ColumnedListPresenter >> showColumnHeaders [

	showColumnHeaders := true
]

{ #category : #'api-events' }
ColumnedListPresenter >> whenColumnsChangedDo: aBlock [
	
	self property: #columns whenChangedDo: aBlock
]

{ #category : #'api-events' }
ColumnedListPresenter >> whenIsResizableChangedDo: aBlock [
	isResizableHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
ColumnedListPresenter >> whenShowColumnHeadersChangedDo: aBlock [
	
	self property: #showColumnHeaders whenChangedDo: aBlock
]
