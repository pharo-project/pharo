examples
exampleSomeClassesSubclassesTree

	| ds ft |

	ds := FTBasicTreeListDataSource new 
		roots: { FTDataSource . String . Boolean } ;
		children: [ :item | item subclasses ];
		sortChildrenBy: [ :a :b | a methods size <= b methods size ];
		display: [ :item :cell | cell addMorphBack: (item name asString , ' (' , item methods size asString , ')') asMorph ];
		yourself.

	ft := FTTableMorph new 
		extent: 200@200;
		dataSource: ds.
	
	ft openInWindow position: 20@10