accessing
project: aProject

	project ifNil: [ self class ensureMetacello ].
	project := aProject