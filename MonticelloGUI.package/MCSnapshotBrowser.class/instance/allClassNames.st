accessing
allClassNames
	^ (items 
		select: [:ea | (ea isOrganizationDefinition | ea isScriptDefinition) not]
		thenCollect: [:ea | ea className]) asSet.
