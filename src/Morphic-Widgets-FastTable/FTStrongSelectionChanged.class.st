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
	#name : #FTStrongSelectionChanged,
	#superclass : #FTAnnouncement,
	#instVars : [
		'selectedRowIndex',
		'event'
	],
	#category : 'Morphic-Widgets-FastTable-Announcement'
}

{ #category : #'instance creation' }
FTStrongSelectionChanged class >> rowIndex: aNumber event: anEvent [
	^ self new 
		selectedRowIndex: aNumber;
		event: anEvent;
		yourself
]

{ #category : #accessing }
FTStrongSelectionChanged >> event [
	^ event 
]

{ #category : #accessing }
FTStrongSelectionChanged >> event: anEvent [
	event := anEvent
]

{ #category : #accessing }
FTStrongSelectionChanged >> selectedRowIndex [
	^ selectedRowIndex
]

{ #category : #accessing }
FTStrongSelectionChanged >> selectedRowIndex: anObject [
	selectedRowIndex := anObject
]
