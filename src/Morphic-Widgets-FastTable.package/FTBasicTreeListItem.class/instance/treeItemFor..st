button-interaction
treeItemFor: anItem

	^FTBasicTreeListItem new 
		datasource: dataSource;
		depth: depth + 1;
		item: anItem