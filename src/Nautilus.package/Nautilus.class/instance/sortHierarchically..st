accessing
sortHierarchically: aBoolean

	sortHierarchically := aBoolean.
	self announcer announce: ( NautilusShowHierarchyChanged boolean: aBoolean )