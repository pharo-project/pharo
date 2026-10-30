"
I am a FindReplace service that allows to use Spec based dialog as a replacement of a native Morphic find & replace dialog. 

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

{ #category : 'event handling' }
SpRubFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [

	self textArea hasSelection
		ifTrue: [ self findText: self textArea selection ].

	self openDialog
]

{ #category : 'event handling' }
SpRubFindReplaceService >> whenTextAreaGotFocus: anAnnounce [
	self textArea: anAnnounce morph
]

{ #category : 'event handling' }
SpRubFindReplaceService >> whenTextAreaLostFocus: anAnnounce [
]
