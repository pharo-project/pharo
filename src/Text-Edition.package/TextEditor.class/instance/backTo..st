typing support
backTo: startIndex
	"Backspace typing"
	self stopIndex > startIndex ifTrue: [
		self selectFrom: startIndex to: self stopIndex - 1.
		self addDeleteSelectionUndoRecord.
		self zapSelectionWith: self nullText].
	^ false


	