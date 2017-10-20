private
shortcutCharacter

	^ self shortcut 
		ifNil: [ nil ]
		ifNotNil: [ :s | s currentCharacter ]