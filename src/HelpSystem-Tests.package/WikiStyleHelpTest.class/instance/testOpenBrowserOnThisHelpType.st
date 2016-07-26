testing
testOpenBrowserOnThisHelpType
	| browser |
	browser := HelpBrowser openOn: WikiStyleHelp.
	World doOneCycleNow.
	browser close