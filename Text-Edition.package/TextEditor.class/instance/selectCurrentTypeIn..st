nonediting/nontyping keys
selectCurrentTypeIn: aKeyboardEvent 
	"Select what would be replaced by an undo (e.g., the last typeIn)."

	self closeTypeIn.
	self selectInterval: self editingState previousInterval.
	^ true