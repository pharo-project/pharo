"
This class defines Help for the system in front of you. It defines the default contents when you open a help browser.
So `SpHelpBrowserPresenter open` is the same as `SpHelpBrowserPresenter openOn: SystemHelp2`.

It identifies the help to be included in the system based on the fact that the class method should be named `asHelpTopic` and that it is tagged with the pragma `<newSystemHelp>` 

```
asHelpTopic 
	<newSystemHelp>

```
"
Class {
	#name : #SystemHelp2,
	#superclass : #Object,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #'private - accessing' }
SystemHelp2 class >> allSystemHelpPragmas [
	^ Pragma allNamed: self pragmaKeyword
]

{ #category : #'instance creation' }
SystemHelp2 class >> asHelpTopic [ 

	|topic helpOnHelp sortedTopics |
	
	topic := HelpTopic named: 'Help'.
	self allSystemHelpPragmas do: [:each | 
		topic subtopics addAll: each method methodClass instanceSide asHelpTopic subtopics
	].
	topic sortSubtopicsByTitle.
	helpOnHelp := topic subtopics detect: [:t | t key = 'HelpOnHelp'] ifNone: [nil].
	helpOnHelp ifNotNil: [  
		sortedTopics := topic subtopics.
		sortedTopics remove: helpOnHelp.
		sortedTopics addLast: helpOnHelp.
		topic subtopics: sortedTopics].
	^topic
]

{ #category : #'private - accessing' }
SystemHelp2 class >> pragmaKeyword [

	^#newSystemHelp

]
