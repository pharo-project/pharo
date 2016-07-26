examples
exampleInspect: anObject

	| ds ft |

	ds := FTBasicTreeListDataSource new .
	
	ds	root: #ROOT->anObject;
		children: [ :item | item value gtInspectorVariableValuePairs ];
		sortChildrenBy: [ :a :b | a key name <= b key name ];
		display: [ :item :cell | cell 
			addMorphBack: item value class systemIcon asMorph;
			addMorphBack: (item key asStringMorph emphasis: 1; yourself);
			addMorphBack: (Morph new color: Color transparent; height: 1; width: 10; yourself);
			addMorphBack: item value asStringMorph ];
		yourself.
	
	ds expand: 1.
	
	ft := FTTableMorph new 
		extent: 800@(World clearArea height // 2);
		dataSource: ds.
	
	ft openInWindow position: 20@10.
	
	^ds