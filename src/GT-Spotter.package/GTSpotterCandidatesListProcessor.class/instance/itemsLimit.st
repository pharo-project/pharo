accessing
itemsLimit
	^ candidatesLimit ifNil: [ self defaultItemsLimit ]