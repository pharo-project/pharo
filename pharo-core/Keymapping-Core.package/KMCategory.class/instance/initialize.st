initialize-release
initialize

	super initialize.
	self class methods
		select: [ :m | m isShortcutDeclaration ]
		thenDo: [ :m | self buildKeymapOnMethod: m ]