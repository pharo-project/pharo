examples
exampleFullInspector
	"An inspector showing all presentations for all objects"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectAllSignatures).
	inspector openOn: Morph new.