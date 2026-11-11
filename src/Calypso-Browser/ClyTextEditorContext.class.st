"
I am default text editor context
"
Class {
	#name : 'ClyTextEditorContext',
	#superclass : 'ClyToolContext',
	#category : 'Calypso-Browser-TextEditors',
	#package : 'Calypso-Browser',
	#tag : 'TextEditors'
}

{ #category : 'public' }
ClyTextEditorContext >> browser [
	^tool browser
]

{ #category : 'public' }
ClyTextEditorContext >> editor [
	^ tool
]

{ #category : 'public' }
ClyTextEditorContext >> selectedTextInterval [
	^tool selectedTextInterval
]
