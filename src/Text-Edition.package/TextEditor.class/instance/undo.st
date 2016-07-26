menu messages
undo
	"Undo  previous edit."
	self closeTypeIn.
	self editingState undo ifFalse: [morph flash]. 
