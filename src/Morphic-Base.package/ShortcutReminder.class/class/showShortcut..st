display
showShortcut: aShortcut
	
	(aShortcut keyText isNil or: [ self enabled not ]) ifTrue: [ ^self ].
	
	self defaultReminder remind: aShortcut.