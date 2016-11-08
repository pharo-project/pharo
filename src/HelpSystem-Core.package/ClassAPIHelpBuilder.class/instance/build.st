building
build
	topicToBuild := HelpTopic named: rootToBuildFrom name.
	topicToBuild iconName: #pageIcon.
	topicToBuild contents: rootToBuildFrom comment.
	addMethods
		ifTrue: [ self buildSubnodesForMethods ].
	addSubclasses
		ifTrue: [ self buildSubnodesForSubclasses ]