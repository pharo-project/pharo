examples
exampleList5
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