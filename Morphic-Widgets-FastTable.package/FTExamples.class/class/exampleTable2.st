examples
exampleTable2
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