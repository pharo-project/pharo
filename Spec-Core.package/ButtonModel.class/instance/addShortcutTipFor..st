private
addShortcutTipFor: aString

	^ self 
		addShortcutTipFor: self shortcutCharacter
		on: aString