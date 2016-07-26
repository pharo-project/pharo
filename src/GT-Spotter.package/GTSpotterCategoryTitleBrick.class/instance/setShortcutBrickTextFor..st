instance creation
setShortcutBrickTextFor: aProcessor
	
	self shortcutBrick text: (ToggleMenuItemShortcut 
		owner: nil 
		keyText: 
			(aProcessor keyBinding printString 
				copyReplaceAll: 'Meta' 
				with: OSPlatform current defaultModifier name)) text