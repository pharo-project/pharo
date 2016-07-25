instance creation
newWithItems: items

	| browser |
	browser := EpLogBrowser newWithViewClass: EpSorterView.
	browser view addWantedItems: items.
	^ browser