examples
exampleProtoObjectClassTree

	| ds ft |

	ds := FTBasicTreeListDataSource new 
		root: ProtoObject;
		children: [ :item | item subclasses ];
		sortChildrenBy: [ :a :b | a name <= b name ];
		display: [ :item :cell | cell addMorphBack: item printString asMorph ];
		yourself.

	ft := FTTableMorph new 
		extent: 200@200;
		dataSource: ds.
	
	ft openInWindow position: 20@10