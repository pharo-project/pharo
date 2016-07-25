examples
exampleStateTree

	| ds ft |

	ds := FTBasicTreeListDataSource new .
	
	ds	roots: { #self -> ds } ;
		children: [ :item | item value gtInspectorVariableValuePairs ];
		sortChildrenBy: [ :a :b | a key name <= b key name ];
		display: [ :item :cell | cell 
			addMorphBack: item value systemIcon asMorph;
			addMorphBack: (item key printString asMorph emphasis: 1; yourself);
			addMorphBack: (Morph new color: Color transparent; height: 1; width: 10; yourself);
			addMorphBack: (item value printString) asMorph ];
		yourself.
		
	ds expand: 3.
	
	ft := FTTableMorph new 
		extent: 600@(World clearArea height - 80);
		dataSource: ds.
	
	ft openInWindow position: 20@10