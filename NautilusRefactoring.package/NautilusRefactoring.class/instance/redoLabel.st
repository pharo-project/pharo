undo-redo
redoLabel
	| stream |
	stream := WriteStream with: 'Redo'.
	self changeManager hasRedoableOperations
		ifFalse: [ ^ stream contents ].
	stream
		nextPut: $ ;
		nextPutAll: self changeManager redoChange name.
	stream position > 40
		ifTrue: [ 
			stream
				position: 40;
				nextPutAll: '...' ].
	^ stream contents