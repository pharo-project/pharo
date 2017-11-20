"
A GLMPresentationsChanged is announced by the pane when the set of presentations changes.
"
Class {
	#name : #GLMPresentationsChanged,
	#superclass : #Announcement,
	#instVars : [
		'pane',
		'oldPresentations'
	],
	#category : #Glamour-Announcements
}

{ #category : #accessing }
GLMPresentationsChanged >> oldPresentations [ 
	"The presentations that were present on the pane before the change." 
	 
	^oldPresentations
]

{ #category : #accessing }
GLMPresentationsChanged >> oldPresentations: aCollection [ 
	"The presentations that were present on the pane before the change." 
	 
	oldPresentations := aCollection
]

{ #category : #accessing }
GLMPresentationsChanged >> pane [ 
	"The pane on which the presentations changed." 
	 
	^pane
]

{ #category : #accessing }
GLMPresentationsChanged >> pane: aPane [ 
	"The pane on which the presentations changed." 
	 
	pane := aPane
]

{ #category : #accessing-convenience }
GLMPresentationsChanged >> presentations [ 
	"The presentationt that are currently present on the pane." 
	 
	^self pane presentations
]

{ #category : #printing }
GLMPresentationsChanged >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' (pane = ', self pane printString , ')'
]
