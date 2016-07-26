api
shortcutString
	| shortcut |
	self keystrokes 
		ifEmpty: [ ^ '' ].
	shortcut := self shortcut.
	^ shortcut asString