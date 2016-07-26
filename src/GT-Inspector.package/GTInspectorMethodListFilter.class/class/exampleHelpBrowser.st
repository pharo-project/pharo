examples
exampleHelpBrowser
	"An inspector for browsing Help pages.
	
	self exampleHelpBrowser
	"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: HelpTopic>>#gtInspectorContentsIn:;
		addAndSelectSignatureFor: HelpTopic>>#gtInspectorSubtopicsIn:).
	inspector openOn: SystemHelp asHelpTopic.