action
realSearch
	self initializeNewSearch.
	dataSource rootsItems do: [ :item | self searchIn: item ] displayingProgress: [ :each | 'Looking inside ' , each name ].
	^ result asArray