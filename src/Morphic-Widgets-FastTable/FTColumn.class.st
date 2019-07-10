"
Description 
------------------
 
I'm a column definition for a fast table.

I keep an id and relevant informations of my behaviour, but this behaviour is better defined in FTDataSource>>#headerColumn:

I can have a sorting strategy, by default I uses a null sorting strategy. The datasource can ask to me informations from the current strategy such as an icon to display in the header cell. When the header is clicked, I'll give a datasource to the strategy and ask to sort it.

Examples
--------------------

	FTTableMorph new
		extent: 650 @ 500;
		addColumn:
			((FTColumn id: 'Icon')
				width: 40;
				yourself);
		addColumn:
			((FTColumn id: 'Name')
				sortableOnProperty: #name;
				yourself);
		addColumn:
			((FTColumn id: 'Number of methods')
				sortableOnProperty: #numberOfMethods;
				yourself);
		addColumn:
			((FTColumn id: 'Number of instance variables')
				sortableOnProperty: #numberOfInstanceVariables;
				yourself);
		dataSource: FTExampleClassInfoTableDataSource new;
		selectFirst;
		showFirstSelection;
		beResizable;
		openInWindow
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	id:							<aSymbol>							An id for the column
	sortingStrategy:		<aColumnSortingStrategy>		A subclass of FTAbstractColumnSortingStrategy that will manage all the behavior of the table sorting
	width:						<anInteger>						An Integer that represent the width of a column

"
Class {
	#name : #FTColumn,
	#superclass : #Object,
	#instVars : [
		'id',
		'width',
		'sortingStrategy'
	],
	#category : #'Morphic-Widgets-FastTable-Model'
}

{ #category : #'instance creation' }
FTColumn class >> id: anObject [
	^ self new
		id: anObject;
		yourself
]

{ #category : #accessing }
FTColumn class >> undefinedColumnWidth [
	"This is a contant that defines a column width is undefined, then the layout will try to arrange 
	 it by itself."
	^ 0
]

{ #category : #layout }
FTColumn >> acquireWidth: anOwnerWidth [
	^ self widthOrUndefined min: anOwnerWidth
]

{ #category : #sorting }
FTColumn >> disableSort [
	self sortingStrategy: (FTNullColumnSortingStrategy column: self)
]

{ #category : #accessing }
FTColumn >> id [
	^ id
]

{ #category : #accessing }
FTColumn >> id: anObject [
	id := anObject
]

{ #category : #initialization }
FTColumn >> initialize [
	super initialize.
	self disableSort
]

{ #category : #testing }
FTColumn >> isSortEnabled [
	^ self sortingStrategy isSortingEnabled
]

{ #category : #initialization }
FTColumn >> resetSorting [
	self sortingStrategy reset
]

{ #category : #accessing }
FTColumn >> sortDataSource: aDataSource [

	"If other columns are already sorted, we need to reset them."
	(aDataSource table columns copyWithout: self) do: #resetSorting.
	self sortingStrategy sortDataSource: aDataSource
]

{ #category : #sorting }
FTColumn >> sortableOnProperty: aProperty [
	self sortingStrategy: (FTPropertyColumnSortingStrategy column: self property: aProperty)
]

{ #category : #sorting }
FTColumn >> sortableUsing: aSortFunction [
	self sortingStrategy: (FTSortFunctionColumnSortingStrategy column: self sortFunction: aSortFunction)
]

{ #category : #accessing }
FTColumn >> sortingIcon [
	^ self sortingStrategy sortingIcon
]

{ #category : #accessing }
FTColumn >> sortingStrategy [
	^ sortingStrategy
]

{ #category : #accessing }
FTColumn >> sortingStrategy: anObject [
	sortingStrategy := anObject
]

{ #category : #accessing }
FTColumn >> transform: anObject [
	^ anObject
]

{ #category : #accessing }
FTColumn >> width [
	^ width
]

{ #category : #accessing }
FTColumn >> width: anObject [
	width := anObject
]

{ #category : #accessing }
FTColumn >> widthOrUndefined [
	^ self width ifNil: [ self class undefinedColumnWidth ]
]
