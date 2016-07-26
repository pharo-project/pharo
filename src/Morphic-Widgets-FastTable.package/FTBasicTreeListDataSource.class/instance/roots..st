construction
roots: aCollection

	rootItems := aCollection collect: [ :each | 
			FTBasicTreeListItem new 
				item: each;
				datasource: self
	].  

	self countAvailableRows