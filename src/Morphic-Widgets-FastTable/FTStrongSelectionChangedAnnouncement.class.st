"
I announce a strong selection happened (this is a double-click)

Description
-------------------

I store the index of the row and the event (doubleClick) the FTTableMorph received.
I should be use by the user to defined an action to execute when a DoubleClic happen on a Table.

Public API and Key Messages
-------------------

- #rowIndex: anIndex event: anEvent 		is the commun constructor.
 
Internal Representation and Key Implementation Points.
------------------

    Instance Variables
	event:		An event received by the FTTableMorph.
	selectedRowIndex:		The index of the row in the table that was selected. 

			
From the index you can get an object from the FTTableMorph with: 

aFTTableMorph dataSource elementAt: selectedRowIndex  

"
Class {
	#name : #FTStrongSelectionChangedAnnouncement,
	#superclass : #FTAnnouncement,
	#instVars : [
		'event',
		'selectedIndex'
	],
	#category : #'Morphic-Widgets-FastTable-Announcement'
}

{ #category : #'instance creation' }
FTStrongSelectionChangedAnnouncement class >> index: aNumber event: anEvent [
	^ self new
		selectedIndex: aNumber;
		event: anEvent;
		yourself
]

{ #category : #'instance creation' }
FTStrongSelectionChangedAnnouncement class >> rowIndex: aNumber event: anEvent [
	self deprecated: 'Use #index:event: instead' transformWith: '`@receiver rowIndex: `@statements1 event: `@statements2' -> '`@receiver index: `@statements1 event: `@statements2'.
	^ self index: aNumber event: anEvent
]

{ #category : #accessing }
FTStrongSelectionChangedAnnouncement >> event [
	^ event 
]

{ #category : #accessing }
FTStrongSelectionChangedAnnouncement >> event: anEvent [
	event := anEvent
]

{ #category : #accessing }
FTStrongSelectionChangedAnnouncement >> selectedIndex [
	^ selectedIndex
]

{ #category : #accessing }
FTStrongSelectionChangedAnnouncement >> selectedIndex: anObject [
	selectedIndex := anObject
]

{ #category : #accessing }
FTStrongSelectionChangedAnnouncement >> selectedRowIndex [
	self deprecated: 'Use #selectedIndex instead' transformWith: '`@receiver selectedRowIndex' -> '`@receiver selectedIndex'.
	^ self selectedIndex
]

{ #category : #accessing }
FTStrongSelectionChangedAnnouncement >> selectedRowIndex: aNumber [
	self deprecated: 'Use #selectedIndex: instead' transformWith: '`@receiver selectedRowIndex: `@statements1' -> '`@receiver selectedIndex: `@statements1'.
	self selectedIndex: aNumber
]
