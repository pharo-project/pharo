"
I represent the mode to use for editing class comments
"
Class {
	#name : #RubSmalltalkCommentMode,
	#superclass : #RubAbstractSmalltalkMode,
	#category : #'Rubric-Editing-Code'
}

{ #category : #accessing }
RubSmalltalkCommentMode class >> label [
	^ 'Smalltalk comment'
]

{ #category : #menu }
RubSmalltalkCommentMode class >> menuKeyword [
	^ 'RubSmalltalkCommentMenu'
]

{ #category : #menu }
RubSmalltalkCommentMode class >> menuOn: aBuilder [
	"Specify the menu used when writing code. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubSmalltalkCommentMenu'
		model: RubEditingArea new) menu popUpInWorld
	"

	<contextMenu>
	<RubSmalltalkCommentMenu>
	RubSmalltalkCodeMode menuOn: aBuilder 
]

{ #category : #helper }
RubSmalltalkCommentMode >> formatMethodCode [
	"do nothing for code comments"
]

{ #category : #testing }
RubSmalltalkCommentMode >> isCompletionEnabled [
	^ false
]
