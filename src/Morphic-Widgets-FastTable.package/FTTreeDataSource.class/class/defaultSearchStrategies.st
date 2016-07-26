accessing
defaultSearchStrategies
	^ Dictionary new
		at: #rootsOnly put: FTRootItemsStrategy;
		at: #default put: FTVisibleItemsStrategy;
		at: #allItems put: FTAllItemsStrategy;
		yourself