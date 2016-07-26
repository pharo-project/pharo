private
wantedItemsFrom: someItems
	
	^ someItems reject: [ :each |
		(items includes: each) or: [ (each isEntryItem) not ] ]