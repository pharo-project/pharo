instance creation
newStaticItemFrom: anItem
	^ FTStaticBasicItem new
		data: anItem data;
		depth: anItem depth;
		children: #();
		dataSource: self;
		yourself