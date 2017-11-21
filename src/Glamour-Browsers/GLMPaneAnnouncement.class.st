"
These announcements are used by the browser to communicate with the renderer whenever the panes configuration is modified in some way.

Instance Variables:
	pane	<GLMPane>
	browser	<GLMBrowser>
	position	<Integer>
"
Class {
	#name : #GLMPaneAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'pane',
		'browser',
		'position'
	],
	#category : #Glamour-Browsers-Announcements
}

{ #category : #'instance creation' }
GLMPaneAnnouncement class >> pane: aPane atPosition: anInteger inBrowser: aBrowser [ 
	 
	^(self new) 
		pane: aPane; 
		position: anInteger; 
		browser: aBrowser; 
		yourself
]

{ #category : #accessing }
GLMPaneAnnouncement >> browser [ 
	 
	^browser
]

{ #category : #accessing }
GLMPaneAnnouncement >> browser: anObject [ 
	 
	browser := anObject
]

{ #category : #accessing }
GLMPaneAnnouncement >> pane [ 
	 
	^pane
]

{ #category : #accessing }
GLMPaneAnnouncement >> pane: anObject [ 
	 
	pane := anObject
]

{ #category : #accessing }
GLMPaneAnnouncement >> position [ 
	 
	^position
]

{ #category : #accessing }
GLMPaneAnnouncement >> position: anObject [ 
	 
	position := anObject
]
