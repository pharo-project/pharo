"
I announce a selection change (usually a click, but can also be done with arrows up and down).

Description
-------------------

I store the old selected rows and the new selected row. 
I should be use to act with the user when he interact with a Fast Table.

Public API and Key Messages
-------------------

- #from: arrayOfIndexes to: arrayOfIndexes 		is the commun constructor.
 
Internal Representation and Key Implementation Points.
------------------

    Instance Variables
	newSelectedRowIndexes:		An array of indexes that store the new selection.
	oldSelectedRowIndexes:		An array of indexes that store the old selection.
			
From the index you can get an object from the FTTableMorph with: 

aFTTableMorph dataSource elementAt: selectedRowIndex 
"
Class {
	#name : #FTSelectionChangedAnnouncement,
	#superclass : #FTAnnouncement,
	#instVars : [
		'newSelectedIndexes',
		'oldSelectedIndexes'
	],
	#category : #'Morphic-Widgets-FastTable-Announcement'
}

{ #category : #'instance creation' }
FTSelectionChangedAnnouncement class >> from: oldArray to: newArray [
	^ self new
		oldSelectedIndexes: oldArray;
		newSelectedIndexes: newArray;
		yourself
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> newSelectedIndexes [
	^ newSelectedIndexes
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> newSelectedIndexes: anObject [
	newSelectedIndexes := anObject
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> newSelectedRowIndexes [
	self deprecated: 'Use #newSelectedIndexes instead' transformWith: '`@receiver newSelectedRowIndexes' -> '`@receiver newSelectedIndexes'.
	^ self newSelectedIndexes
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> newSelectedRowIndexes: anObject [
	self deprecated: 'Use #newSelectedIndexes: instead' transformWith: '`@receiver newSelectedRowIndexes: `@statements1' -> '`@receiver newSelectedIndexes: `@statements1'.
	self newSelectedIndexes: anObject
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> oldSelectedIndexes [
	^ oldSelectedIndexes
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> oldSelectedIndexes: anObject [
	oldSelectedIndexes := anObject
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> oldSelectedRowIndexes [
	self deprecated: 'Use #oldSelectedIndexes instead' transformWith: '`@receiver oldSelectedRowIndexes' -> '`@receiver oldSelectedIndexes'.
	^ self oldSelectedIndexes
]

{ #category : #accessing }
FTSelectionChangedAnnouncement >> oldSelectedRowIndexes: anObject [
	self deprecated: 'Use #oldSelectedIndexes: instead' transformWith: '`@receiver oldSelectedRowIndexes: `@statements1' -> '`@receiver oldSelectedIndexes: `@statements1'.
	self oldSelectedIndexes: anObject
]
