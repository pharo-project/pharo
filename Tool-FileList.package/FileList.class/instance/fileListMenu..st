file list menu
fileListMenu: aMenu

	reference
		ifNil: [^ self noFileSelectedMenu: aMenu]
		ifNotNil: [^ self fileSelectedMenu: aMenu].
