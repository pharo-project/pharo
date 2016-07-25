construction
addToggle: aString target: anObject selector: aSymbol getStateSelector: stateSymbol enablementSelector: enableSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	|item|
	item := ToggleMenuItemMorph new
		contents: aString;
		target: anObject;
		selector: aSymbol;
		arguments: argList;
		getStateSelector: stateSymbol;
		enablementSelector: enableSymbol.
	^ self addMenuItem: item
