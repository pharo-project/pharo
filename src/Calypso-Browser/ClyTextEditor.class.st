"
I am used to override standard text editor operations in Calypso way.
I override all kind of navigation requests like sendersOfIt, implementorsOfIt and others.
I always call my actual browser owner to perform requested navigation query
"
Class {
	#name : 'ClyTextEditor',
	#superclass : 'RubSmalltalkEditor',
	#category : 'Calypso-Browser-TextEditors',
	#package : 'Calypso-Browser',
	#tag : 'TextEditors'
}

{ #category : 'context menu' }
ClyTextEditor class >> contextMenuOn: aBuilder [
	<RubSmalltalkCodeMenu>

	| context |
	(aBuilder model editingMode isKindOf: ClyTextEditingMode)
		ifFalse: [ ^self ].

	context := aBuilder model browserTool createCommandContext.
	CmdContextMenuActivation registerContextMenuOn: aBuilder inContext: context
]

{ #category : 'accessing' }
ClyTextEditor >> browser [
	^textArea editingMode browser
]

{ #category : 'accessing' }
ClyTextEditor >> browserTool [
	^textArea editingMode browserTool
]

{ #category : 'operations' }
ClyTextEditor >> cancel [

	self browserTool cancelChanges
]

{ #category : 'new selection' }
ClyTextEditor >> notify: aString at: anInteger in: aStream [
	"Because in calypso there is error icons and UI feedback absent in others RubEditor,
	there no need to insert -> message in the text area"
	"Note: error reporting can still be improved, but this need more work"

	| pos message |
	message := (aString endsWith: ' ->')
		ifTrue: [ aString allButLast: 3 ]
		ifFalse: [ aString ].

	self inform: message.
	pos := self selectionInterval notEmpty
		ifTrue: [ self startIndex + anInteger - 1 ]
		ifFalse: [ anInteger ].
	self selectFrom: pos to: pos - 1. "move cursor in front of the location"
	textArea flashColor: self theme dangerBackgroundColor
]

{ #category : 'do-its' }
ClyTextEditor >> printIt [
	"Treat the current text selection as an expression; evaluate it. Insert the
	description of the result of evaluation after the selection and then make
	this description the new text selection."

	| pointerArea |
	self evaluateSelectionAndDo: [ :result |
		| popoverPresenter |
		pointerArea := textArea cursor boundsInWorld.

		popoverPresenter := SpPopoverPresenter new presenter:
			                    (SpCodePopoverPrintPresenter
				                     newCode: self
				                     object: result).
		popoverPresenter relativeTo: (SpPresenter new
				 layout: SpBoxLayout new;
				 build;
				 yourself).
		popoverPresenter bePositionRight.
		popoverPresenter popupPointingTo: pointerArea.
		popoverPresenter takeKeyboardFocus ]
]

{ #category : 'operations' }
ClyTextEditor >> undo [

	super undo.

	self editingState undoManager hasPrevious ifFalse: [
		self browserTool changesCancelled]
]
