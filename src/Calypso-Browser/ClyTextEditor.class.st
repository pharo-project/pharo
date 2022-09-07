"
I am used to override standard text editor operations in Calypso way.
I override all kind of navigation requests like sendersOfIt, implementorsOfIt and others.
I always call my actual browser owner to perform requested navigation query
"
Class {
	#name : #ClyTextEditor,
	#superclass : #RubSmalltalkEditor,
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #'context menu' }
ClyTextEditor class >> contextMenuOn: aBuilder [
	<RubSmalltalkCodeMenu>

	| context |
	(aBuilder model editingMode isKindOf: ClyTextEditingMode)
		ifFalse: [ ^self ].
		
	context := aBuilder model browserTool createCommandContext.
	CmdContextMenuActivation registerContextMenuOn: aBuilder inContext: context
]

{ #category : #accessing }
ClyTextEditor >> browser [
	^textArea editingMode browser
]

{ #category : #accessing }
ClyTextEditor >> browserTool [
	^textArea editingMode browserTool
]

{ #category : #operations }
ClyTextEditor >> cancel [

	self browserTool cancelChanges
]

{ #category : #operations }
ClyTextEditor >> printIt [
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."

	| printString |
	self
		evaluateSelectionAndDo: [ :result | 
			printString := [ result asScriptResultStringInCalypso ]
				on: Error
				do: [ '<error in printString: try ''Inspect it'' to debug>' ].
			self afterSelectionInsertAndSelect: printString ]
]

{ #category : #operations }
ClyTextEditor >> undo [

	super undo.

	self editingState undoManager hasPrevious ifFalse: [ 
		self browserTool changesCancelled]
]
