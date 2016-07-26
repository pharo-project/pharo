examples
exampleRawInspector
	"An inspector showing only the Raw presentation for all objects"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: Object>>#gtInspectorRawIn:).
	inspector openOn: Morph new.