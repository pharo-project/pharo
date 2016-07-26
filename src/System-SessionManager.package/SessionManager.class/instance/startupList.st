accessing
startupList
	^ (categories flatCollect: #prioritizedList)
		asArray