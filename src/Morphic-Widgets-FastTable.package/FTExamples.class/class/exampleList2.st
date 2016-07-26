examples
exampleList2
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
