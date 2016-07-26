undo-redo private
addTypeInUndoRecord
	| begin stop undoText redoText |
	begin := self startOfTyping min: self stopIndex. 
	stop := self stopIndex max: self startOfTyping.
	self editingState previousInterval: (begin to: stop - 1). 
	undoText := self nullText.
	redoText := stop > begin
			ifTrue: [self text copyFrom: begin to: stop - 1]
			ifFalse: [self nullText].
	((undoText isEmpty and: [redoText isEmpty]) and: [self editingState previousInterval size < 1])
			ifFalse: [self 
				redoArray: { textArea. #redoTypeIn:interval:.  {redoText. begin to: begin + self selection size - 1} } 
				undoArray: {textArea. #undoTypeIn:interval:.  {undoText. begin to: stop - 1} }].
	
