menu messages
redo
	"redo previous edit"

	self editingState redo ifFalse: [morph flash]. 
