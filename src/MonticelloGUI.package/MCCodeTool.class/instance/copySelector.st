menus
copySelector
	"Copy the selected selector to the clipboard"

	| selector |
	(selector := self selectedMessageName) ifNotNil:
		[Clipboard clipboardText: selector asString]