loading and updating
updateFromSystem
	| i |
	i := 0.
	self prepareForUpdating.
	'FreeType' displayProgressFrom: 0 to: 3 during:[ :mainBar |
		self updateFileCacheInfo.
		mainBar current: 1.
		self updateFontsFromSystem.
		mainBar current: 2.
		self updateAvailableFontFamilies.
		mainBar current: 3 ].
	LogicalFont allInstances do:[:each | each clearRealFont]. "in case they have a bad one"
			