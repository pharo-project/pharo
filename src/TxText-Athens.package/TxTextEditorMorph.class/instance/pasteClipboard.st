editing
pasteClipboard
	| stuff |
	
	stuff := Clipboard clipboardText asString.
	
	self insert: stuff.