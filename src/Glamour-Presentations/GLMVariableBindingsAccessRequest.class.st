"
Request to perform an action on the current bindings from the editor.
"
Class {
	#name : #GLMVariableBindingsAccessRequest,
	#superclass : #Announcement,
	#instVars : [
		'actionBlock'
	],
	#category : 'Glamour-Presentations-Announcements'
}

{ #category : #accessing }
GLMVariableBindingsAccessRequest >> action [

	^ actionBlock
]

{ #category : #accessing }
GLMVariableBindingsAccessRequest >> action: aBlock [

	actionBlock := aBlock
]
