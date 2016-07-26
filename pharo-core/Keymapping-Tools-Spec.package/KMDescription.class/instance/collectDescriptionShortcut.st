initialization
collectDescriptionShortcut
	collectingBlock := [ :e | {(e description asString) . ':' . (e shortcut asString)} ]