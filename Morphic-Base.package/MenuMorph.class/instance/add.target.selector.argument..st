construction
add: aLabelString target: target selector: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	^ self add: aLabelString
		target: target
		selector: aSymbol
		argumentList: (Array with: arg)
