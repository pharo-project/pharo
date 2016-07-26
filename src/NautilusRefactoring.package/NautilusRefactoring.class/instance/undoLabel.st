undo-redo
undoLabel
	| stream |
	stream := WriteStream with: 'Undo'.
	self changeManager hasUndoableOperations
		ifFalse: [ ^ stream contents ].
	stream
		nextPut: $ ;
		nextPutAll: self changeManager undoChange name.
	stream position > 40
		ifTrue: [ 
			stream
				position: 40;
				nextPutAll: '...' ].
	^ stream contents