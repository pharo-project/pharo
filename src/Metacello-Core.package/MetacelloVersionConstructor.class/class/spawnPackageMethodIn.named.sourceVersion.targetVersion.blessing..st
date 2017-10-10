deprecated
spawnPackageMethodIn: aConfig named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString blessing: blessing

	self deprecated: 'see MetacelloToolBox class>>createDevelopment:for:importFromBaseline:description: for a similar example'.
	^self new
		spawnPackageMethodIn: aConfig 
		category: 'versions'
		named: newSelector 
		sourceVersion: sourceVersionString 
		targetVersion: targetVersionString
		blessing: blessing