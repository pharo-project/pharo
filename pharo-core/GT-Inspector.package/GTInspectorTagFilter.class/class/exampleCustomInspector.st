examples
exampleCustomInspector
	"An inspector showing only the custom presentations for all object (excluding Raw and Meta)."
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorTagFilter new
		defaultTag: #custom;
		addAndSelectTag: #custom).
	inspector openOn: Morph new.