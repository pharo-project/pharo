A HistoryIterator holds an UndoRedoGroup in order to store an history of commands with the possibility of undoing and redoing. The iterator cursor is represented by the index inst var. Index always contains the position of the command that can be currently undone. So, undo decrease index and redo increase index. When a new record is stored, then, index contains the newly added record position.
See HistoryIteratorTest for examples.

Instance Variables
	index:		<Integer>
	maxSize:		<Integer>
	plugged:		<Boolean>
	recorder:		<UndoRedoGroup>

index
	- the iterator cursor

maxSize
	- the maximum number of records that can be added in the root group.

plugged
	- if false, then adding of command is not allowed. Useful to prevent bad history recording recurssions (record while undoing or redoing).

recorder
	- The root of the history tree which records undo/redo commands
