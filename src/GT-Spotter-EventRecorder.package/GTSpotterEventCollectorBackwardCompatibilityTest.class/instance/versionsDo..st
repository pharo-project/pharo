enumerating
versionsDo: aBlock
	self allPragmaVersions
		do: [ :eachPragmaVersion | 
			| announcement |
			announcement := self materializePragmaVersion: eachPragmaVersion.
			aBlock cull: announcement cull: eachPragmaVersion methodSelector ]