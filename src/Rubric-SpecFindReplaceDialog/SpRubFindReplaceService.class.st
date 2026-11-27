"
I am the find and replace service of the IDE code editors (see BaselineOfIDE).

Cmd+F opens the inline find bar inherited from FindReplaceService (RubFindReplaceBar), attached to the focused editor. When a text area cannot hold the inline bar, such as a single line text field, I open the Spec dialog SpRubFindReplaceDialog instead.

I can currently work only with the default Morphic Spec backend.
"
Class {
	#name : 'SpRubFindReplaceService',
	#superclass : 'FindReplaceService',
	#instVars : [
		'dialog',
		'dialogWindow'
	],
	#category : 'Rubric-SpecFindReplaceDialog',
	#package : 'Rubric-SpecFindReplaceDialog'
}

{ #category : 'accessing' }
SpRubFindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := RubAbstractTextArea caseSensitiveFinds]
]

{ #category : 'accessing' }
SpRubFindReplaceService >> dialog [

	^ dialog
]

{ #category : 'accessing' }
SpRubFindReplaceService >> dialogWindow [

	^ dialogWindow
]

{ #category : 'event handling' }
SpRubFindReplaceService >> discardDialog [

	dialog := nil.
	dialogWindow := nil
]

{ #category : 'event handling' }
SpRubFindReplaceService >> openDialog [

	| text |
	text := findText asString.
	dialog := SpRubFindReplaceDialog on: self.
	dialog updateFromModel.
	(text isNotEmpty and: [ text ~= dialog findInput text ]) ifTrue: [
		dialog findInput text: text ].
	dialogWindow := dialog openDialog 
]

{ #category : 'find bar' }
SpRubFindReplaceService >> openFallbackFindDialog [
	"The text area cannot hold the inline find bar (a single line text field for example): open the Spec dialog."

	self openDialog
]
