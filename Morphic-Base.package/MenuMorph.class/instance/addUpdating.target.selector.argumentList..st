construction
addUpdating: wordingSelector target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the initial wording of the menu item is obtained by sending the wordingSelector to the target. If the wording prefixed with <yes> or <no>, the on/off state of the menu item will reflect it."
 
	|aString str|
	aString := (MessageSend receiver: target selector: wordingSelector) valueWithEnoughArguments: argList.
	self flag: #CleanYesNoOnOffAfter.
	(aString isKindOf: Association)
	     ifTrue: [aString := aString value]
		ifFalse: [
				str := aString readStream.
				(str skipTo: $>) ifTrue: [aString := str upToEnd]].
	self
		addToggle: aString
		target: target
		selector: aSymbol
		getStateSelector: wordingSelector
		enablementSelector: nil
		argumentList: argList
