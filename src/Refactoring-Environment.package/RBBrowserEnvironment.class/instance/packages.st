accessing-packages
packages

	^ self packageOrganizer packages
		select: [ :package |
			self includesCategory: package name ]