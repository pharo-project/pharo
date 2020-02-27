"
This class defines Help for the system in front of you.
It defines the default contents when you open a help browser.

So ""HelpBrowser open"" is the same as ""HelpBrowser openOn: SystemHelp"".



"
Class {
	#name : #SystemHelp,
	#superclass : #Object,
	#classInstVars : [
		'pragmaCollector',
		'helpTopic'
	],
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #'private accessing' }
SystemHelp class >> allSystemHelpPragmas [
	^ self pragmaCollector collected

]

{ #category : #accessing }
SystemHelp class >> asHelpTopic [ 

	^ helpTopic ifNil: [ helpTopic := self createHelpTopic].

]

{ #category : #reset }
SystemHelp class >> createHelpTopic [
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
	^topic

]

{ #category : #accessing }
SystemHelp class >> pragmaCollector [
	^ pragmaCollector
		ifNil: [ pragmaCollector := (PragmaCollector
				filter: [ :prag | prag selector = self pragmaKeyword ])
				reset;
				whenChangedDo: [ self resetHelpTopic ];
				yourself ]
]

{ #category : #'private accessing' }
SystemHelp class >> pragmaKeyword [

	^#systemHelp

]

{ #category : #reset }
SystemHelp class >> resetHelpTopic [

	helpTopic := self createHelpTopic 
]
