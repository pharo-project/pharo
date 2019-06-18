"
I am default text editor context
"
Class {
	#name : #ClyTextEditorContext,
	#superclass : #CmdToolContext,
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #public }
ClyTextEditorContext >> browser [
	^tool browser
]

{ #category : #public }
ClyTextEditorContext >> selectedTextInterval [
	^tool selectedTextInterval
]
