"
This announcement is emitted when the top window switches from ""this"" window to another. 
The new window will announce WindowActivated, while the old will announce this annoucnement.
"
Class {
	#name : #WindowDeActivated,
	#superclass : #WindowAnnouncement,
	#category : #'Morphic-Widgets-Windows-Announcements'
}

{ #category : #testing }
WindowDeActivated >> isDeActivated [

	^true
]
