accessing
rootItems: aListOfItems
	"set the root items as in superclass with a copy + keep the list locally"
	initialRootItems := aListOfItems.
	super rootItems: initialRootItems copy.
	self updateList