testing
doesOverride: aCodeChange 

	^ aCodeChange isEpCategoryChange and: [
		aCodeChange affectedPackageName = self affectedPackageName ]