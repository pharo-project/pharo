examples
exampleList3
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