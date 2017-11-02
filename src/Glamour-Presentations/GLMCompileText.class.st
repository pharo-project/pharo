"
I am event indicating to the renderer to compile the code of the given presentation.
"
Class {
	#name : #GLMCompileText,
	#superclass : #Announcement,
	#instVars : [
		'context',
		'session',
		'presentation',
		'action'
	],
	#category : 'Glamour-Presentations-Announcements'
}

{ #category : #accessing }
GLMCompileText >> action [

	^ action
]

{ #category : #accessing }
GLMCompileText >> action: aBlock [

	action := aBlock
]

{ #category : #accessing }
GLMCompileText >> context [
	
	^ context
]

{ #category : #accessing }
GLMCompileText >> context: anObject [
	
	context := anObject
]

{ #category : #accessing }
GLMCompileText >> presentation [

	^ presentation
]

{ #category : #accessing }
GLMCompileText >> presentation: anObject [

	presentation := anObject
]

{ #category : #accessing }
GLMCompileText >> session [
	
	^ session
]

{ #category : #accessing }
GLMCompileText >> session: anObject [
	
	session := anObject
]
