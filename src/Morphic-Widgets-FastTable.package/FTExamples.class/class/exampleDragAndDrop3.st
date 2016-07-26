examples
exampleDragAndDrop3
	"Show drag&drop between two lists, multiple selection. 
	 Showing 50 elements, so we can test Cmd+A (select all)"
	<example>
	| list1 list2 |

	list1 := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		addColumn: (FTColumn id: 'Drag from me!');
		dataSource: (FTExampleDataSource elements: ((Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]) first: 50));
		dragEnabled: true;
		beMultipleSelection;
		yourself.
		
	list2 := FTTableMorph new 
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		addColumn: (FTColumn id: 'Drop into me!');
		dataSource: FTExampleDropDataSource new;
		dropEnabled: true;
		yourself.
		
	^ PanelMorph new
		extent: 600@350;
		layoutPolicy: TableLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		listDirection: #leftToRight;
		cellPositioning: #topLeft;
		listCentering: #topLeft;
		wrapCentering: #topLeft;
		cellInset: 5;
		borderWidth: 0;
		addMorphBack: list1;
		addMorphBack: list2;
		openInWindow
