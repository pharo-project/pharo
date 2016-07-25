construction
add: aLabelString target: anObject selector: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	^ self add: aLabelString
		target: anObject
		selector: aSymbol
		argumentList: EmptyArray.
