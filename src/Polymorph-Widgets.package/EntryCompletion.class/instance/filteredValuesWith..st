accessing
filteredValuesWith: aToken
	^ (self dataSourceBlock value: aToken) 
		select: [:v | aToken isNil or: [v isNil or: [self filterBlock value: v value: aToken]]]