accessing
showHierarchy: aBoolean

	showHierarchy := aBoolean.
	self announcer announce: ( NautilusShowPackagesChanged boolean: aBoolean not )