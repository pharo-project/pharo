"
This class defines Help for the system in front of you.
It defines the default contents when you open a help browser.

So ""HelpBrowser open"" is the same as ""HelpBrowser openOn: SystemHelp"".



"
Class {
	#name : #SystemHelp,
	#superclass : #Object,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #'private accessing' }
SystemHelp class >> allSystemHelpPragmas [
	^(PragmaCollector filter: [:prag | prag keyword = self pragmaKeyword ]) reset collected

]

{ #category : #conversion }
SystemHelp class >> asHelpTopic [ 
	|topic helpOnHelp sortedTopics |
	topic := HelpTopic named: 'Help'.
	self allSystemHelpPragmas do: [:each | 
		topic subtopics addAll: each method methodClass instanceSide asHelpTopic subtopics
	].
	topic sortSubtopicsByTitle.
	helpOnHelp := topic subtopics detect: [:t | t key = 'HelpOnHelp'] ifNone: [self error: 'Help for the help system is removed'].
	sortedTopics := topic subtopics.
	sortedTopics remove: helpOnHelp.
	sortedTopics addLast: helpOnHelp.
	topic subtopics: sortedTopics.
	^topic.

]

{ #category : #'private accessing' }
SystemHelp class >> pragmaKeyword [

	^#systemHelp

]
