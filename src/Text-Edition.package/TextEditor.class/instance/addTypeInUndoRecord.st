undo-redo private
addTypeInUndoRecord
	| begin stop undoText redoText undoRec |
	begin := self startOfTyping min: self stopIndex. 
	stop := self stopIndex max: self startOfTyping.
	self editingState 
		previousInterval: (begin to: stop - 1) 
		selection: (self text copyFrom: begin to: stop - 1). 
	undoText := self nullText.
	redoText := stop > begin
			ifTrue: [self text copyFrom: begin to: stop - 1]
			ifFalse: [self nullText].
	((undoText isEmpty and: [redoText isEmpty]) and: [self editingState previousInterval size < 1])
			ifFalse: [self editingState
				redoArray: { morph. #redoTypeIn:interval:.  {redoText. begin to: begin + self selection size - 1} } 
				undoArray: {morph. #undoTypeIn:interval:.  {undoText. begin to: stop - 1} }].
	
