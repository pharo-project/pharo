construction
addToggle: aString target: anObject selector: aSymbol getStateSelector: stateSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self addToggle: aString
		target: anObject
		selector: aSymbol
		getStateSelector: stateSymbol
		argumentList: EmptyArray