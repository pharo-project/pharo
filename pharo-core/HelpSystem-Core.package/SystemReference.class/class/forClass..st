help topic creation
forClass: aClass
	|root topic |
	root := HelpTopic named: 'System reference for ', aClass name.
	topic := ClassAPIHelpBuilder buildHelpTopicFrom: aClass.
	root addSubtopic: topic.
	^root