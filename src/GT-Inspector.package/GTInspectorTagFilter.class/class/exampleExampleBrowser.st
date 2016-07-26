examples
exampleExampleBrowser
	"A browser for viewing examples"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorTagFilter new
		addAndSelectTag: #examples).
	inspector openOn: Dictionary.