api
accept
	| shortcut |
	shortcut := self shortcut.
	keymapSetting accept: shortcut.
	self initialShortcut: shortcut.
	edited := false.