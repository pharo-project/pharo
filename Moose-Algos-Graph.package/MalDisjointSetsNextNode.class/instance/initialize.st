accessing
initialize

	self flag: #refactor.
	self makeSet. "should be called in super initialize"
	nextNodes := OrderedCollection new