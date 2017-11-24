"
A new pane was removed
"
Class {
	#name : #GLMPaneRemoved,
	#superclass : #GLMPaneAnnouncement,
	#category : #'Glamour-Browsers-Announcements'
}

{ #category : #'instance creation' }
GLMPaneRemoved class >> pane: aPane fromBrowser: aBrowser [ 
	 
	^(self new) 
		pane: aPane; 
		browser: aBrowser; 
		yourself
]
