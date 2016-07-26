*Keymapping-Core
isShortcutDeclaration

	^self pragmas anySatisfy: [ :p | p keyword = #shortcut ]