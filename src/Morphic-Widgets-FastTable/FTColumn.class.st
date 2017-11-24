"
I'm a column definition for a fast  table.

Description 
------------------
 
I keep an id and relevant information of my behaviour, but this behaviour is better defined in FTDataSource>>#headerTable:column:
 
Internal Representation and Key Implementation Points.
-----------------
    Instance Variables
	id:		An id for the column
	width:		An Integer that represent the width of a column.

"
Class {
	#name : #FTColumn,
	#superclass : #Object,
	#instVars : [
		'id',
		'width'
	],
	#category : #'Morphic-Widgets-FastTable'
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

{ #category : #accessing }
FTColumn >> id [
	^ id
]

{ #category : #initialization }
FTColumn >> id: anObject [
	id := anObject
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
