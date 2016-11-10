*Keymapping-Core
isShortcutDeclaration
	^ self pragmas anySatisfy: [ :p | p selector = #shortcut ]