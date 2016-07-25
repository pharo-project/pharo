testing
testOpenBrowserOnThisHelpType
	| browser |
	browser := HelpBrowser openOn: CustomHelp.
	World doOneCycleNow.
	browser close