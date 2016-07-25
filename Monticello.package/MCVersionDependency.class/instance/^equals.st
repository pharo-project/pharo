comparing
= other
	^ other species = self species
		and: [other versionInfo = versionInfo
				and: [other package = package]]