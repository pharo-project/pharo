construction
add: aLabelString selector: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	self add: aLabelString
		target: defaultTarget
		selector: aSymbol
		argumentList: EmptyArray.
