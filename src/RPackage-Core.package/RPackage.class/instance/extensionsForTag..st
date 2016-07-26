class tags
extensionsForTag: aRPackageTag

	^ self extensionMethods select: [ :extensionMethod | | category |
		category := extensionMethod protocol allButFirst.
		self category:  category matches: aRPackageTag categoryName ]