private
wrapItem: aKMShortcutDeclaration
	| name categorie |
	name := aKMShortcutDeclaration shortcutName.
	categorie := aKMShortcutDeclaration categoryName.
	^ {aKMShortcutDeclaration realValue shortcut asString . name.
	categorie}