accessing
currentPageLastIndex
	^ pageSize ifNil: [self nodeList size] ifNotNil: [(self currentPageFirstIndex + pageSize - 1) min: self nodeList size]