accessing
repositories
	"Answer the configured monticello repositories."

	|  result |
	result := OrderedCollection withAll: repositories.
	packageCacheRepository ifNotNil: [ result addFirst: packageCacheRepository ].
	^ result asArray