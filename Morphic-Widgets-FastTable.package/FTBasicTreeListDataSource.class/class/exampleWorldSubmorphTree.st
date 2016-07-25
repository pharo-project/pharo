examples
exampleWorldSubmorphTree

	| ds ft |

	ds := FTBasicTreeListDataSource new 
		root: World;
		children: [ :item | item submorphs ];
		expand: 2;
		display: [ :item :cell | cell addMorphBack: item printString asMorph ];
		yourself.

	ft := FTTableMorph new 
		extent: 400@(World clearArea height - 80);
		dataSource: ds.
	
	ft openInWindow position: 20@10