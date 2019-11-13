"
I am an abtract class to define the basic implementation of a DataSource. A DataSource is a Model for a FTTableMorph. My purpose is to provide content to it .

Description
--------------------

My subclasses will store some values that will be use to feed a FTTableMorph.
 I work with a FTTableMorph so I store one.

Public API and Key Messages
--------------------

- #elementAt: anIndex 		return an object to display for an index in the table.
		
- #numberOfRows 			return the max number of rows inside the table.
			
- #cellColumn: index1 row: idex2 		return a Morph (probably a FTCellMorph) that will be display in the table. The Morph need to contains the object at index2 and everything the user want to display for a data.
		
- #newDataSourceMatching: aRegex / #searchText: aString 		these methods are use with a FTFunction. For more information see FTSeachFunction or FTFilterFunction for mone information.

Example
-------------------

This is an Abstract class, see subclasses for examples.
 
Internal Representation and Key Implementation Points.
-------------------

    Instance Variables
	table:		A FTTableMorph that use the dataSource.

"
Class {
	#name : #FTDataSource,
	#superclass : #Object,
	#instVars : [
		'table'
	],
	#category : #'Morphic-Widgets-FastTable-DataSource'
}

{ #category : #accessing }
FTDataSource >> basicHeaderCellFor: column [
	| cell |
	column id ifNil: [ ^ nil ].
	cell := FTHeaderColumnCellMorph new
		listCentering: #center;
		column: column;
		cellInset: 5;
		addMorph: column id asStringMorph;
		yourself.

	column sortingIcon ifNotNil: [ :icon | cell addMorph: icon ].

	^ cell
]

{ #category : #accessing }
FTDataSource >> cellColumn: column row: rowIndex [
	"Answer a morph with the cell view. I will probably return a FTCellMorph."

	self subclassResponsibility
]

{ #category : #'drag and drop' }
FTDataSource >> dropElements: aCollection index: rowIndex [
	^ false
]

{ #category : #private }
FTDataSource >> elementAt: rowIndex [
	"I am a really important method for a DataSource. I take an index and I return an object 
	 that should be displayed in the table for the index."
	self subclassResponsibility
]

{ #category : #accessing }
FTDataSource >> headerColumn: column [
	"Answers a cell who will be the header of a column"

	^ nil
]

{ #category : #accessing }
FTDataSource >> menuColumn: column row: rowIndex [
	"Answer a menu for the (column, row) provided"

	^ nil
]

{ #category : #accessing }
FTDataSource >> newDataSourceMatching: aFTFilter [
	"I am here to help the FTFilterFunction. I take a aFTFilter and I need to return a new dataSource with filtered elements. Each dataSource is responsible of his filtering in order to get a generic mechanism."
	
	"the default action is to just do nothing, return self"
	
	^self
]

{ #category : #accessing }
FTDataSource >> numberOfRows [
	"I return the number of elements I can display in the table when I am call."

	^ self subclassResponsibility
]

{ #category : #'drag and drop' }
FTDataSource >> passengerAt: rowIndex [
	"Answer 'passenger' for drag&drop operation (a passenger is the object who will travel between 
	 drag and drop). By default, passenger is same as current element"
	^ self elementAt: rowIndex
]

{ #category : #private }
FTDataSource >> readyToBeDisplayed [
	"I am a method call when I am set to a FTTableMorph. I can be override, for example the dataSource of glamour send an announcement to update the dataSource of a Refderer."

	
]

{ #category : #accessing }
FTDataSource >> realElementAt: anIndex [
	"I am here because some dataSource wrap there element into an other Object but we still want to access to the initial element. In that case a DataSource can override me to get the real element."

	^ self elementAt: anIndex
]

{ #category : #accessing }
FTDataSource >> rowHeight: roIndex [

	^ self table rowHeight
]

{ #category : #accessing }
FTDataSource >> searchText: aString [
	"search a text and answers a list of elements indexes matching condition. 
	 E.g,
	 	#() 		--> No match 
		#(1) 		--> match first element
		#(1 42) 	--> match elements 1 and 42
			
	Rows not in range will cause the end of the world as we know it :)"
	^ #()
]

{ #category : #sorting }
FTDataSource >> sortElements: aSortFunction [
	"This method should sort the elements of the datasource using the sort function as parameter."

	self subclassResponsibility
]

{ #category : #sorting }
FTDataSource >> sortWith: aSortFunction [
	self sortElements: aSortFunction.
	self table refresh
]

{ #category : #accessing }
FTDataSource >> table [
	"this the table binded to this dataSource"

	^ table
]

{ #category : #accessing }
FTDataSource >> table: anObject [
	table := anObject
]

{ #category : #private }
FTDataSource >> tableRefresh [
	^ self table refresh
]

{ #category : #accessing }
FTDataSource >> toString: anObject [
	"Override me if you expect something else."

	^ anObject asString
]

{ #category : #sorting }
FTDataSource >> unsort [
	self unsortElements.
	self table refresh
]

{ #category : #sorting }
FTDataSource >> unsortElements [
	"This method should return the elements in their initial state."

	self subclassResponsibility
]

{ #category : #'drag and drop' }
FTDataSource >> wantsDropElements: aCollection type: type index: rowIndex [
	^ false
]
