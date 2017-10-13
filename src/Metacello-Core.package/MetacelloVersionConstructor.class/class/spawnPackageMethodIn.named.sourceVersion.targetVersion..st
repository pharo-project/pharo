deprecated
spawnPackageMethodIn: aConfig named: newSelector sourceVersion: sourceVersionString targetVersion: targetVersionString

	self deprecated: 'see MetacelloToolBox class>>createDevelopment:for:importFromBaseline:description: for a similar example'.
	^self 
		spawnPackageMethodIn: aConfig 
		category: 'versions'
		named: newSelector 
		sourceVersion: sourceVersionString 
		targetVersion: targetVersionString 
		blessing: #development