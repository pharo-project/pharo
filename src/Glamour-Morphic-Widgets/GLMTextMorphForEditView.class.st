"
GLMTextMorphForEditView is meant to work with GLMPluggableTextMorph.
"
Class {
	#name : #GLMTextMorphForEditView,
	#superclass : #TextMorphForEditView,
	#category : #Glamour-Morphic-Widgets
}

{ #category : #editing }
GLMTextMorphForEditView >> acceptContents [
	self flag: 'Hack for disable the CTRL+S behaviour'
]

{ #category : #private }
GLMTextMorphForEditView >> editorClass [
	"Answer the class used to create the receiver's editor"
	
	^GLMSmalltalkEditor
]

{ #category : #callbacks }
GLMTextMorphForEditView >> keyStroke: anEvent [
	| isHandledByModel |
	isHandledByModel := self editView model keystroke: anEvent from: self editView.
	isHandledByModel ifFalse: [ super keyStroke: anEvent ]
]
