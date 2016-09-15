instance creation
newWithItems: items

	| browser |
	browser := self newWithViewClass: EpSorterView.
	browser view addWantedItems: items.
	^ browser