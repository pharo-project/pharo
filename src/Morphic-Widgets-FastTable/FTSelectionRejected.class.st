"
I announce a rejected selection change (e.g. an attempt to a row/column or cell outside the bounds of the table)

Description
-------------------

I store the direction of the change.

Public API and Key Messages
-------------------

•	#withDirection: is the sole constructor 


Internal Representation and Key Implementation Points.
------------------

Instance Variables
direction:	Direction of the attempted selection
"
Class {
	#name : #FTSelectionRejected,
	#superclass : #FTAnnouncement,
	#instVars : [
		'direction'
	],
	#category : #'Morphic-Widgets-FastTable-Announcement'
}

{ #category : #'instance creation' }
FTSelectionRejected class >> withDirection: aDirection [
	^ self new
		direction: aDirection;
		yourself
]

{ #category : #accessing }
FTSelectionRejected >> direction [

	^ direction
]

{ #category : #accessing }
FTSelectionRejected >> direction: anObject [

	direction := anObject
]
