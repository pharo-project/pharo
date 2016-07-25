pager managing
pageSize: aPageSize 
	self pager
		ifNil: [aPageSize ifNotNil: [self addNavigationPane: (MorphTreePager on: self pageSize: aPageSize)]]
		ifNotNil: [aPageSize 
			ifNil: [self removePager]
			ifNotNil: [self pager changePageSize: aPageSize]]