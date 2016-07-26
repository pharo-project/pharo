examples
exampleList1
	"Show a list with all Object methods"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@550;
		dataSource: (FTExampleMethodListDataSource for: Object);
		yourself.
		
	^ list openInWindow
