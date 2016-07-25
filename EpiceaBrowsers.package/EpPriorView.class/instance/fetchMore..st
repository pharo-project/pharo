refreshing
fetchMore: aFetchMoreItem 

	desiredNumberOfItems := desiredNumberOfItems + self pageSize.
	
	rootItems 
		remove: aFetchMoreItem ifAbsent: [Warning signal: aFetchMoreItem asString , 'is not found !'] ;
		addAllLast: (self next: self pageSize from: aFetchMoreItem entryReference).