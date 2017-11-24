"
Used by PluggableTextEditorMorph to specify a plain TextEditor for the editor

"
Class {
	#name : #TextMorphForEditorView,
	#superclass : #TextMorphForEditView,
	#category : #'Polymorph-Widgets'
}

{ #category : #private }
TextMorphForEditorView >> editorClass [
	^ TextEditor
]
