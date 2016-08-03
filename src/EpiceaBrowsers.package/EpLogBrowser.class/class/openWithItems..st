instance creation
openWithItems: items

	^ (self newWithItems: items)
		refresh;
		open;
		yourself