examples
exampleFileBrowser
	"An inspector for browsing directories and files."
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: FileReference>>#gtInspectorStScriptIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorPngIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorJpegIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorItemsIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorGifIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorContentsIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorFuelIn:).
	inspector openOn: FileSystem workingDirectory.