refreshing
fetch: aNumber from: aFetchMoreItem 

	desiredNumberOfItems := desiredNumberOfItems + aNumber.
	
	rootItems 
		remove: aFetchMoreItem ifAbsent: [Warning signal: aFetchMoreItem asString , 'is not found !'] ;
		addAllLast: (self next: aNumber from: aFetchMoreItem entryReference).