"
A list of examples. 
See class side.
"
Class {
	#name : #FTExamples,
	#superclass : #Object,
	#category : #'Morphic-Widgets-FastTable-Examples'
}

{ #category : #examples }
FTExamples class >> example1 [
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits 
			sorted: [ :a :b | a name < b name]));
		yourself.
	list openInWindow

]

{ #category : #examples }
FTExamples class >> example2 [
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@300;
		dataSource: (FTExampleDataSource elements: ((Smalltalk allClassesAndTraits 
			sorted: [ :a :b | a name < b name])
			first: 10));
		yourself.
	list openInWindow

]

{ #category : #examples }
FTExamples class >> exampleDragAndDrop1 [
	"Show drag&drop between two lists"
	<example>
	| list1 list2 |

	list1 := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		addColumn: (FTColumn id: 'Drag from me!');
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		dragEnabled: true;
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

]

{ #category : #examples }
FTExamples class >> exampleDragAndDrop2 [
	"Show drag&drop between two lists, multiple selection"
	<example>
	| list1 list2 |

	list1 := FTTableMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		addColumn: (FTColumn id: 'Drag from me!');
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
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

]

{ #category : #examples }
FTExamples class >> exampleDragAndDrop3 [
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

]

{ #category : #examples }
FTExamples class >> exampleDragAndDrop4 [
	"Show drag&drop in one list, with autoscroll"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@300;
		addColumn: (FTColumn id: 'Drag from & to me!');
		dataSource: (FTExampleDropDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		dragEnabled: true;
		dropEnabled: true;
		yourself.
		
	^ list openInWindowLabeled: 'Autoscrolling'
]

{ #category : #examples }
FTExamples class >> exampleEasyList1 [
	"Show an easy list with all Morph methods"
	<example>
	| list |

	list := FTEasyListMorph new
		extent: 300@550;
		elements: Morph methods;
		icon: [ :method | (FTExampleMethodModel method: method) actionOn: nil ];
		"display: [ :method | method selector ];	"	
		yourself.
		
	^ list openInWindow

]

{ #category : #examples }
FTExamples class >> exampleEasyList2 [
	"Show an easy list with all Morph methods and a simple menu"
	<example>
	| list |

	list := FTEasyListMorph new
		extent: 300@550;
		elements: Morph methods;
		icon: [ :method | (FTExampleMethodModel method: method) actionOn: nil ];
		display: [ :method | method selector ];		
		menu: [ :method | 
			MenuMorph new 
				add: ('Browse #', (method methodClass name), '>>#', method selector)
					target: method
					selector: #browse;
				add: ('Inspect #', (method methodClass name), '>>#', method selector)
					target: method
					selector: #inspect;
				yourself ];
		yourself.
		
	^ list openInWindow

]

{ #category : #examples }
FTExamples class >> exampleEasyList3 [
	"Show an easy list with all Morph methods and a simple menu, with a list header"
	<example>
	| list |

	list := FTEasyListMorph new
		extent: 300@550;
		elements: Morph methods;
		header: 'Method list';
		icon: [ :method | (FTExampleMethodModel method: method) actionOn: nil ];
		display: [ :method | method selector ];		
		menu: [ :method | 
			MenuMorph new 
				add: ('Browse #', (method methodClass name), '>>#', method selector)
					target: method
					selector: #browse;
				add: ('Inspect #', (method methodClass name), '>>#', method selector)
					target: method
					selector: #inspect;
				yourself ];
		yourself.
		
	^ list openInWindow

]

{ #category : #examples }
FTExamples class >> exampleEditableList1 [
	<example>
	| list data|
	data := OrderedCollection with: 'A' with: 'B' with: 'C'.
	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleDropDataSource new elements: data);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | |index morph ed |
				index := ann selectedRowIndex.
				morph := list visibleRowMorphAtIndex: index.
				ed := RubFloatingEditorBuilder new
					customizeEditorWith: [ :editor | 
						editor scrollbarsShowNever.
						editor bounds: (morph bounds insetBy: (Margin left: -2 top: 0 right: 0 bottom: 0  ))
					];
				withEditedContentsDo: [ :editedContents | 
					data at: index put: editedContents asString.
					list refresh].
				ed openEditorWithContents: (data at: index) 
		];
		yourself.
		
	^ list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleFilterableList [
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource:
			(FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilter;
		yourself.
	list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleList1 [
	"Show a list with all Object methods"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleMethodListDataSource for: Object);
		yourself.
		
	^ list openInWindow

]

{ #category : #examples }
FTExamples class >> exampleList2 [
	"Show a list with all Object methods and preselects one item"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleMethodListDataSource for: Object);
		selectRowIndex: 42;
		showFirstRowSelection;
		allowDeselection;
		yourself.

	^ list openInWindow

]

{ #category : #examples }
FTExamples class >> exampleList3 [
	"Show a list with all Object methods, 
		- preselects one element
	 	- responds to selection changes"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleMethodListDataSource for: Object);
		selectRowIndex: 42;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('row selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		yourself.
		
	^ list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleList4 [
	"Show a list with all Object methods, 
		- preselects one element 
		- respond to selection changes
		- respond to strong selections (double click)"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleMethodListDataSource for: Object);
		selectRowIndex: 42;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('row selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		yourself.
		
	^ list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleList5 [
	"Show a list with all Object methods, 
		- preselects one element 
		- respond to selection changes
		- respond to strong selections (double click)
		- allows multiple selection"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleMethodListDataSource for: Object);
		selectRowIndexes: #(42 43);
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^ list openInWindow
]

{ #category : #'function examples' }
FTExamples class >> exampleListWithExplicitFilter [
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilter;
		explicitFunction;
		yourself.
	list openInWindow
]

{ #category : #'function examples' }
FTExamples class >> exampleListWithExplicitFilterAndActionButton [
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilterWithAction: [ :dataSource :searchPattern | Transcript crShow: searchPattern ] named: 'Show';
		yourself.
	list openInWindow
]

{ #category : #'function examples' }
FTExamples class >> exampleListWithExplicitFilterAndActionButton2 [
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		enableFilterWithAction: [ :dataSource :searchPattern :selectedRow | selectedRow browse ];
		yourself.
	list openInWindow
]

{ #category : #'function examples' }
FTExamples class >> exampleListWithExplicitSearch [
	<example>
	| list |
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource:
			(FTExampleDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		explicitFunction;
		yourself.
	list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleOutline1 [
	<example>
	| list |
	list := FTTableMorph new
		extent: 200 @ 400;
		dataSource:
			(FTOutlineDataSource
				items:
					{(1 to: 10).
					(11 to: 20).
					(21 to: 30).
					(31 to: 40).
					(41 to: 50)}).
	list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleOutline2 [
	<example>
	| data list |
	data := OrderedCollection
		withAll:
			(Morph allSubclasses
				collect: [ :each | each allSubclasses sorted: [ :a :b | a name < b name ] ]).
	list := FTTableMorph new
		extent: 300 @ 300;
		dataSource: (FTOutlineDataSource items: data).
	list openInWindow
]

{ #category : #examples }
FTExamples class >> exampleTable1 [
	"Show a table with all Object methods"
	<example>
	| table |

	table := FTTableMorph new
		extent: 650@500;
		addColumn: (FTColumn id: 'Name');
		addColumn: (FTColumn id: 'Protocol');
		addColumn: (FTColumn id: 'Origin');
		dataSource: (FTExampleMethodTableDataSource for: Object);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^ table openInWindow
]

{ #category : #examples }
FTExamples class >> exampleTable2 [
	"Show a table with all Object methods, with a fixed width"
	<example>
	| table |

	table := FTTableMorph new
		extent: 650@500;
		addColumn: ((FTColumn id: 'Name') width: 350);
		addColumn: (FTColumn id: 'Protocol');
		addColumn: (FTColumn id: 'Origin');
		dataSource: (FTExampleMethodTableDataSource for: Morph);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^ table openInWindow
]

{ #category : #examples }
FTExamples class >> exampleTable3 [
	"Show a table with all Object methods, with a header"
	<example>
	| table |

	table := FTTableMorph new
		extent: 650@500;
		addColumn: ((FTColumn id: '#') width: 40);
		addColumn: ((FTColumn id: 'Name') width: 350);
		addColumn: (FTColumn id: 'Protocol');
		addColumn: (FTColumn id: 'Origin');
		intercellSpacing: 1;
		dataSource: (FTExampleMethodTableDataSource for: Morph);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^ table openInWindow
]

{ #category : #examples }
FTExamples class >> exampleTable4 [
	"Show a table with all Object methods, with a resizable header"
	<example>
	| table |

	table := FTTableMorph new
		extent: 650@500;
		addColumn: ((FTColumn id: '#') width: 40);
		addColumn: ((FTColumn id: 'Name') width: 350);
		addColumn: (FTColumn id: 'Protocol');
		addColumn: (FTColumn id: 'Origin');
		hideColumnHeaders;
		beResizable;
		intercellSpacing: 8@1;
		dataSource: (FTExampleMethodTableDataSource for: Morph);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^ table openInWindow
]

{ #category : #examples }
FTExamples class >> exampleTableHorizontalScroll [
	"Show a table with all Object methods, with a header,
	 all fixed width columns as horizontal scroll."
	<example>
	"self exampleTableHorizontalScroll"
	| table |
	
	table := FTTableMorph newTrialHorizontalScrollBar
		extent: 650@500;
		addColumn: ((FTColumn id: '#') width: 40);
		addColumn: ((FTColumn id: 'Name') width: 350);
		addColumn: ((FTColumn id: 'Protocol') width: 200);
		addColumn: ((FTColumn id: 'Origin') width: 200);
		intercellSpacing: 1;
		dataSource: (FTExampleMethodTableDataSource for: Morph);
		selectRowIndex: 1;
		showFirstRowSelection;
		onAnnouncement: FTSelectionChanged 
			do: [ :ann | ('rows selected: ', (ann newSelectedRowIndexes asString)) crLog ];
		onAnnouncement: FTStrongSelectionChanged 
			do: [ :ann | ('double-click on row: ', (ann selectedRowIndex asString)) crLog ];
		beMultipleSelection;
		yourself.
		
	^table openInWindow.
]

{ #category : #'tree examples' }
FTExamples class >> exampleTree1 [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots:
			((ProtoObject allSubclasses reject: [ :e | e name endsWith: 'class' ])
				sort: [ :a :b | a name < b name ])
		children: [ :data | data subclasses sort: [ :a :b | a name < b name ] ].
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTree2 [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 1000)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTreeWithAllItemsFilter [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 10)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #allItems.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		enableFilter;
		explicitFunction;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTreeWithAllItemsSearch [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 10)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #allItems.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTreeWithRootFilter [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 1000)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #rootsOnly.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		enableFilter;
		explicitFunction;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTreeWithRootSearch [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 1000)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #rootsOnly.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTreeWithVisibleItemsFilter [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 1000)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #default.	"Optional"
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		enableFilter;
		explicitFunction;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleTreeWithVisibleItemsSearch [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots: (1 to: 1000)
		children: [ :data :item | 
			item depth even
				ifTrue: [ 1 to: data / 2 ]
				ifFalse: [ 1 to: data - 1 ] ].
	ds searchStrategy: #default.	"Optional"
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		explicitFunction;
		openInWindow
]

{ #category : #'tree examples' }
FTExamples class >> exampleexampleOneSubLevelTree [
	<example>
	| ds |
	ds := FTTreeDataSource
		roots:
			((ProtoObject allSubclasses reject: [ :e | e name endsWith: 'class' ])
				sort: [ :a :b | a name < b name ])
		children: [ :item | item subclasses sort: [ :a :b | a name < b name ] ].
	ds maxDepth: 1.
	FTTableMorph new
		extent: 200 @ 400;
		dataSource: ds;
		openInWindow
]

{ #category : #'see class side' }
FTExamples >> seeClassSide [
]
