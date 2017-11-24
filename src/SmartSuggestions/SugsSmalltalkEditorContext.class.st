"
I am a context that know how to work with the SmalltalkEditor.
"
Class {
	#name : #SugsSmalltalkEditorContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #selection }
SugsSmalltalkEditorContext >> selectedClass [
	^model model model owner selectedBehavior
]

{ #category : #selection }
SugsSmalltalkEditorContext >> selectedMethod [
	^model model model owner selectedMessage.
]

{ #category : #refactoring }
SugsSmalltalkEditorContext >> sourceTextArea [
	^ model model widget.
]

{ #category : #tools }
SugsSmalltalkEditorContext >> tools [
	^ model systemNavigation
]
