construction
addWithLabel: aLabelString enablement: anEnablementSelector action: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is constant, and the optional enablementSelector determines whether or not the item should be enabled."

	self flag: #ToDeprecate.
	self
		addToggle: aLabelString
		target: defaultTarget
		selector: aSymbol
		getStateSelector: nil
		enablementSelector: anEnablementSelector
		argumentList: EmptyArray