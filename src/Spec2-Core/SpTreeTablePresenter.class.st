"
A presenter for tree with a table.
A table has columns with a type (See column types section).

**IMPORTANT:** A table presenter does not contain other presenters. Instead, it maps the model provided (See `SpTreeTablePresenter>>#roots:` and `SpTreeTablePresenter>>#children:`) with the columns defined. If you need this behavior, you will need to use `SpComponentListPresenter`.

"
Class {
	#name : #SpTreeTablePresenter,
	#superclass : #SpAbstractTreePresenter,
	#instVars : [
		'#columns => SpObservableSlot',
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot',
		'#lazilyComputeChildren'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #specs }
SpTreeTablePresenter class >> adapterName [

	^ #TreeTableAdapter
]

{ #category : #documentation }
SpTreeTablePresenter class >> addDocumentExtraSections: aBuilder [

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Column types' ] withLevel: 2.
	aBuilder unorderedListDuring: [ 
		(SpTableColumn allSubclasses sorted: #name ascending) do: [ :eachType | 
			aBuilder item: [
				aBuilder monospace: eachType name ] ] ]
]

{ #category : #documentation }
SpTreeTablePresenter class >> documentFactoryMethodSelector [

	^ #newTreeTable
]

{ #category : #documentation }
SpTreeTablePresenter class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'Examples'. self class methods select: [ :each | each protocol = self documentExamplesProtocol ].
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'API Selection Methods'. self methods select: [ :each | each protocol = #'api-selection' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	 }
]

{ #category : #api }
SpTreeTablePresenter >> addColumn: aColumn [
	"Add a column to the table. A column should be an instance of `SpTableColumn`"

	columns := self columns copyWith: aColumn
]

{ #category : #api }
SpTreeTablePresenter >> beNotResizable [
	"Mark the table as 'not resizable', which means there will be not possibility to resize the 
	 columns of it."

	self isResizable: false
]

{ #category : #api }
SpTreeTablePresenter >> beResizable [
	"Mark the table as 'resizable', which means there will be a slider to resize the columns."

	self isResizable: true
]

{ #category : #api }
SpTreeTablePresenter >> columns [
	"Answer the columns composing this table."

	^ columns
]

{ #category : #api }
SpTreeTablePresenter >> columns: aCollection [
	"Set all columns at once. 
	 `aCollection` is a list of instances of `SpTableColumn`"

	columns := aCollection
]

{ #category : #api }
SpTreeTablePresenter >> hideColumnHeaders [
	"Hide the column headers"

	showColumnHeaders := false
]

{ #category : #initialization }
SpTreeTablePresenter >> initialize [
	super initialize.

	self withScrollBars.
	
	activationBlock := [ ].
	
	showColumnHeaders := true.
	columns := #().
	isResizable := false.
	childrenBlock := [ :item | #() ].
	roots := #().
	lazilyComputeChildren := false.
	verticalAlignment := SpVerticalAlignment new.


	self beSingleSelection.
	self activateOnDoubleClick.
	self registerEvents
]

{ #category : #testing }
SpTreeTablePresenter >> isResizable [
	"Answer true if table allows resizing of its columns."

	^ isResizable
]

{ #category : #private }
SpTreeTablePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTreeTablePresenter >> isShowingColumnHeaders [
	"Answer true if the table is configured to show column headers."

	^ showColumnHeaders
]

{ #category : #private }
SpTreeTablePresenter >> lazilyComputeChildren [
	"When we compute lazily the children, we will show in all cases the expand arrow even if there is no children until we open this children."

	lazilyComputeChildren := true
]

{ #category : #private }
SpTreeTablePresenter >> lazilyComputeChildren: aBoolean [
	lazilyComputeChildren := aBoolean
]

{ #category : #private }
SpTreeTablePresenter >> shouldLazilyComputeChildren [
	^ lazilyComputeChildren
]

{ #category : #api }
SpTreeTablePresenter >> showColumnHeaders [
	"Show column headers"

	showColumnHeaders := true
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenColumnsChangedDo: aBlock [
	"Inform when columns have changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"	
	

	self property: #columns whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenIsResizableChangedDo: aBlock [
	"Inform when resizable property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #isResizable whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectedItemChangedDo: aBlock [
	"Inform when selected index has changed. 
	 `aBlock` receives one optional argument (the new element)."
	
	self whenSelectionChangedDo: [ :selection | 
		aBlock cull: (selection ifNotNil: [ :anArray | anArray second ]) ]
]
