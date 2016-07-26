menus
copySelectionToClipboard
	"Copy my selected item to the clipboard as a string"

	self selection ifNotNil: [ Clipboard clipboardText: self selection asString ] ifNil: [ self flash ]