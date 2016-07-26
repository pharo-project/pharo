initializing
initialize
	super initialize.
	self content: ''.
	self creationTime: DateAndTime now.
	title := nil.
	persistenceStrategies := OrderedCollection with: (GTPlayPageCachePersistence new page: self)