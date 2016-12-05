class tags
basicRemoveTag: tag
	classTags remove: tag ifAbsent: [].
	
	SystemAnnouncer uniqueInstance 
		classTagRemoved: tag name fromPackage: self