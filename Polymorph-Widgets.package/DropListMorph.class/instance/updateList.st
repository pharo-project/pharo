updating
updateList
	"Refresh the list."

	self getListSelector isSymbol ifTrue: [
		self list: (self model perform: self getListSelector).
		listSelectionIndex := 0]