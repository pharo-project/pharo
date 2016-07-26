obsolete
removeObsoleteEventHandlers
	scroller submorphs do:[:m|
		m eventHandler: nil; highlightForMouseDown: false; resetExtension].