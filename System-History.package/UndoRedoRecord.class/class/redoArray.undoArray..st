instance creation
redoArray: doArray undoArray: undoArray
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