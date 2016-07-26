conversion
asHelpTopic 
	|topic helpOnHelp sortedTopics |
	topic := HelpTopic named: 'Help'.
	self allSystemHelpPragmas do: [:each | 
		topic subtopics addAll: each method methodClass theNonMetaClass asHelpTopic subtopics
	].
	topic sortSubtopicsByTitle.
	helpOnHelp := topic subtopics detect: [:t | t key = 'HelpOnHelp'] ifNone: [self error: 'Help for the help system is removed'].
	sortedTopics := topic subtopics.
	sortedTopics remove: helpOnHelp.
	sortedTopics addLast: helpOnHelp.
	topic subtopics: sortedTopics.
	^topic.
