"
A GLMMatchingPresentationsChanged is announced by the pane when the presentations that should be displayed change due to changes in the values of the ports of the pane.
"
Class {
	#name : #GLMMatchingPresentationsChanged,
	#superclass : #Announcement,
	#instVars : [
		'pane',
		'oldMatchingPresentations'
	],
	#category : #'Glamour-Announcements'
}

{ #category : #'accessing-convenience' }
GLMMatchingPresentationsChanged >> matchingPresentations [ 
	"The presentations that match now."
	^ self pane matchingPresentations
]

{ #category : #accessing }
GLMMatchingPresentationsChanged >> oldMatchingPresentations [ 
	"The presentations that matched before this announcement." 
	 
	^oldMatchingPresentations
]

{ #category : #accessing }
GLMMatchingPresentationsChanged >> oldMatchingPresentations: aCollection [ 
	"The presentations that matched before this announcement." 
	 
	oldMatchingPresentations := aCollection
]

{ #category : #accessing }
GLMMatchingPresentationsChanged >> pane [ 
	"The pane on which the matching presentations changed." 
	 
	^pane
]

{ #category : #accessing }
GLMMatchingPresentationsChanged >> pane: aPane [ 
	"The pane on which the matching presentations changed." 
	 
	pane := aPane
]

{ #category : #printing }
GLMMatchingPresentationsChanged >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' (pane = ', self pane printString , ')'
]
