selection
selection: item
	"Called from outside to request setting a new selection."

	self selectionIndex: (self getList indexOf: item)