"
An UndoRedoRecord is a leaf of an history tree. It  holds undo and redo commands. Such a command is represented by a MessageSend. 

Instance Variables
	redoMessage:		<MessageSend>
	undoMessage:		<MessageSend>

redoMessage
	- The command which is performed for redoing

undoMessage
	- The command which is performed for undoing

"
Class {
	#name : #UndoRedoRecord,
	#superclass : #HistoryLeaf,
	#instVars : [
		'redoMessage',
		'undoMessage'
	],
	#category : #'System-History-Utilities'
}

{ #category : #compatibility }
UndoRedoRecord class >> do: doMessageSend undo: undoMessageSend [
	^ self redo: doMessageSend undo: undoMessageSend
]

{ #category : #compatibility }
UndoRedoRecord class >> doArray: doArray undoArray: undoArray [
	^ self redoArray: doArray undoArray: undoArray
]

{ #category : #'instance creation' }
UndoRedoRecord class >> redo: doMessageSend undo: undoMessageSend [

	| anUndoRecord |
	anUndoRecord := self new.
	anUndoRecord redoMessage: doMessageSend.
	anUndoRecord undoMessage: undoMessageSend.
	^anUndoRecord.
]

{ #category : #'instance creation' }
UndoRedoRecord class >> redoArray: doArray undoArray: undoArray [
	" doArray and undoArray are constructed as follow: #(receiver selector arguments) "
	| anUndoRecord |
	anUndoRecord := self new.
	anUndoRecord redoMessage: (MessageSend 
		receiver: 	(doArray first) 
		selector: 	(doArray second) 
		arguments: 	(doArray third)).
	undoArray 
		ifNotNil: [ 
			anUndoRecord undoMessage: (MessageSend 
				receiver: 	(undoArray first) 
				selector: 	(undoArray second) 
				arguments: 	(undoArray third))].
	^anUndoRecord.
]

{ #category : #compatibility }
UndoRedoRecord >> do [
	^self redo
]

{ #category : #compatibility }
UndoRedoRecord >> doMessage [

	^ self redoMessage
]

{ #category : #compatibility }
UndoRedoRecord >> doMessage: aMessageSend [
	^ self redoMessage: aMessageSend

]

{ #category : #'redo-undo' }
UndoRedoRecord >> redo [
	^self redoMessage value.
]

{ #category : #accessing }
UndoRedoRecord >> redoMessage [

	^ redoMessage
]

{ #category : #accessing }
UndoRedoRecord >> redoMessage: aMessageSend [

	redoMessage := aMessageSend
]

{ #category : #'redo-undo' }
UndoRedoRecord >> undo [
	^self undoMessage
		ifNotNil: [self undoMessage value].
]

{ #category : #accessing }
UndoRedoRecord >> undoMessage [

	^ undoMessage
]

{ #category : #accessing }
UndoRedoRecord >> undoMessage: aMessageSend [

	undoMessage := aMessageSend
]
