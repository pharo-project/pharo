examples
exampleBasicInspect: anObject

	| ds ft |

	ds := FTBasicTreeListDataSource new .
	
	ds	root: #ROOT->anObject;
		children: [ :item | item value gtInspectorVariableValuePairs ];
		sortChildrenBy: [ :a :b | a key name <= b key name ];
		display: [ :item :cell | cell addMorphBack: item printString asMorph ];
		yourself.
	
	ft := FTTableMorph new 
		extent: 600@(World clearArea height - 80);
		dataSource: ds.
	
	ft openInWindow position: 20@10.
	
	^ds