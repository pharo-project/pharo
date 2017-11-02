"
A PragmaAnnouncement is announced by a PragmaCollector when it adds, removes or updates a PragmaHandler.

Instance Variables
	pragma:		<Pragma>

pragma
	- the pragma which has been added, removed or updated

"
Class {
	#name : #PragmaAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'pragma'
	],
	#category : 'PragmaCollector-Announcements'
}

{ #category : #'instance creation' }
PragmaAnnouncement class >> pragma: aPragma [
	^ self new pragma: aPragma
]

{ #category : #accessing }
PragmaAnnouncement >> pragma [
	^ pragma
]

{ #category : #accessing }
PragmaAnnouncement >> pragma: aPragma [
	pragma := aPragma
]
