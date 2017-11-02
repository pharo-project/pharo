"
I represent the mode to use for editing class comments
"
Class {
	#name : #RubSmalltalkCommentMode,
	#superclass : #RubAbstractSmalltalkMode,
	#category : 'Rubric-Editing-Code'
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

{ #category : #initialize-release }
RubSmalltalkCommentMode >> editorClass [
	^ RubSmalltalkEditor.

]

{ #category : #initialize-release }
RubSmalltalkCommentMode >> formatMethodCode [
	"do nothing for code comments"
]

{ #category : #'as yet unclassified' }
RubSmalltalkCommentMode >> isCodeCompletionAllowed [
	^ false
]

{ #category : #initialize-release }
RubSmalltalkCommentMode >> updateTextAreaWhenPlugged [
	super updateTextAreaWhenPlugged

]

{ #category : #initialize-release }
RubSmalltalkCommentMode >> updateTextAreaWhenUnplugged [
	super updateTextAreaWhenUnplugged

]
