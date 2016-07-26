action
newStaticItemFrom: anItem
	^ FTStaticBasicItem new
		data: anItem data;
		depth: anItem depth;
		children: #();
		yourself