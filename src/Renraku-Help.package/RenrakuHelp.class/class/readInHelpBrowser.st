as yet unclassified
readInHelpBrowser

	^ HelpBrowser openOn: (
		HelpTopic new
			addSubtopic: self asHelpTopic;
			yourself)