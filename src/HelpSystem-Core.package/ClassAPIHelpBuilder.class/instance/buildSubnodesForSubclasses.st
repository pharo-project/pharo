private building
buildSubnodesForSubclasses
	| topic |
	rootToBuildFrom subclasses isEmpty
		ifTrue: [ ^ self ].
	topicToBuild iconName: #bookIcon.
	topic := subclassesAsSeparateTopic
		ifTrue: [ topicToBuild addSubtopic: (HelpTopic named: 'Subclasses') ]
		ifFalse: [ topicToBuild ].
	rootToBuildFrom subclasses
		do:
			[ :subclass | topic addSubtopic: (self buildSubclassTopicFor: subclass) ].
	topic sortSubtopicsByTitle