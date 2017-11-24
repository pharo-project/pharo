"
I indicate to the renderer to execute a message on the text editor morph that contains the actual text.
"
Class {
	#name : #GLMTextEditorRequest,
	#superclass : #Announcement,
	#instVars : [
		'action'
	],
	#category : #'Glamour-Presentations-Announcements'
}

{ #category : #'instance creation' }
GLMTextEditorRequest class >> for: aSelector [

	^ self new action: [ :editor | editor perform: aSelector ]
]

{ #category : #accessing }
GLMTextEditorRequest >> action [
	^ action
]

{ #category : #accessing }
GLMTextEditorRequest >> action: anObject [
	action := anObject
]
