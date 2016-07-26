examples
exampleList4
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