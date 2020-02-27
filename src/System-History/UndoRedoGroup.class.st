"
An UndoRedoGroup is a composite node of an history tree. It is made to allow redoing or undoing of several commands in one shot. See for example its use to record all text replacement when a ""replace all"" is performed from a FindReplaceService (see TextEditor>>replaceAll: with: startingAt:)

Instance Variables

"
Class {
	#name : #UndoRedoGroup,
	#superclass : #HistoryNode,
	#category : #'System-History-Utilities'
}

{ #category : #'undo-undo' }
UndoRedoGroup >> do [
	self redo
]

{ #category : #'undo-undo' }
UndoRedoGroup >> redo [
	self history do: [:i | i redo]
]

{ #category : #'undo-undo' }
UndoRedoGroup >> undo [
	self history reverseDo: [:i | i undo]
]
