construction
addToggle: aString selector: aSymbol
	"Append a menu item with the given label. If the item is selected,
	it will send the given selector to the default target object."

	self addToggle: aString
		target: defaultTarget
		selector: aSymbol
		getStateSelector: nil
		argumentList: EmptyArray