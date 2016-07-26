system integration
hasPackageMatchingExtensionName: anExtensionName

	(self hasPackageExactlyMatchingExtensionName: anExtensionName)
		ifTrue: [ ^true ].
		
	packages keysDo: [ :aSymbol | 
		(anExtensionName beginsWithEmpty: aSymbol caseSensitive: false)
			ifTrue: [ ^ true]].
	^ false
	