pager managing
chunkSize: aPageSize 
	self pager
		ifNil: [aPageSize ifNotNil: [self addNavigationPane: (MorphTreeChunkPager on: self pageSize: aPageSize)]]
		ifNotNil: [aPageSize 
			ifNil: [self removePager]
			ifNotNil: [self pager changePageSize: aPageSize]]