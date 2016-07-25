accessing
urlProvider: aUrlObject
	urlProvider := aUrlObject ifNotNil: [ aUrlObject asZnUrl ]