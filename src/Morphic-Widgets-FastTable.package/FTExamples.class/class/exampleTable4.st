examples
exampleTable4
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