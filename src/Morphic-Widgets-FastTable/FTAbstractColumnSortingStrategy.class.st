"
Description
--------------------

I am an abstract classes to define the API of FastTable column sorting strategies. 

My subclasses will implement different way to sort a FastTable.

I am linked to a FTColumn implementing the Strategy design pattern.

Public API and Key Messages
--------------------

- #isSortingEnable 		Return a boolean to know if the sorting through this column is currently enabled.
- #sortDatasource: 		Sort a datasource depending on my state. This behavior will be defined by my subclasses.
- #sortingIcon 				Return an icon that can be displayed in the header cell to indicated in which state we are.
	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	column:		<aFTColumn>	 The column using me to sort its table.

"
Class {
	#name : #FTAbstractColumnSortingStrategy,
	#superclass : #Object,
	#instVars : [
		'column'
	],
	#category : #'Morphic-Widgets-FastTable-Sorting'
}

{ #category : #accessing }
FTAbstractColumnSortingStrategy class >> column: aColumn [
	^ self new
		column: aColumn;
		yourself
]

{ #category : #accessing }
FTAbstractColumnSortingStrategy >> column [
	^ column
]

{ #category : #accessing }
FTAbstractColumnSortingStrategy >> column: anObject [
	column := anObject
]

{ #category : #testing }
FTAbstractColumnSortingStrategy >> isSortingEnabled [
	^ true
]

{ #category : #accessing }
FTAbstractColumnSortingStrategy >> morphicIconNamed: aString [
	^ (self iconNamed: aString) asMorph
]

{ #category : #initialization }
FTAbstractColumnSortingStrategy >> reset [
	self subclassResponsibility
]

{ #category : #sorting }
FTAbstractColumnSortingStrategy >> sortDataSource: aDataSource [
	self subclassResponsibility
]

{ #category : #accessing }
FTAbstractColumnSortingStrategy >> sortingIcon [
	^ self subclassResponsibility
]
