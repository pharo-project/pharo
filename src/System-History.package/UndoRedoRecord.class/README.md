An UndoRedoRecord is a leaf of an history tree. It  holds undo and redo commands. Such a command is represented by a MessageSend. 

Instance Variables
	redoMessage:		<MessageSend>
	undoMessage:		<MessageSend>

redoMessage
	- The command which is performed for redoing

undoMessage
	- The command which is performed for undoing
