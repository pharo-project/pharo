"
I indicate to the renderer to execute a message on the window that rendered this presentation. It will only execute if the presentation is directly rendered in a window (i.e., if this is the presentation to which #openWith: was sent)
"
Class {
	#name : #GLMWindowRequest,
	#superclass : #Announcement,
	#instVars : [
		'action'
	],
	#category : #Glamour-Core
}

{ #category : #'instance creation' }
GLMWindowRequest class >> for: aSelector [

	^ self new action: [ :editor | editor perform: aSelector ]
]

{ #category : #accessing }
GLMWindowRequest >> action [
	^ action
]

{ #category : #accessing }
GLMWindowRequest >> action: anObject [
	action := anObject
]
