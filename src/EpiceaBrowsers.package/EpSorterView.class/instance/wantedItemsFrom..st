private
wantedItemsFrom: someItems

	^ someItems select: [:each |
		each isEntryItem and: [(items includes: each) not] ]