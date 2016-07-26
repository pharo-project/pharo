initialization
on: aConfig

	| cacheKey |
	cacheKey := aConfig class.
	project := MetacelloPlatform current
		stackCacheFor: #versionConstructor
		at: cacheKey
		doing: [ :cache | 
			self calculate: aConfig project: nil.
			cache at: cacheKey put:  self project ].
	self setProject: project.