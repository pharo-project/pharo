building
build
	topicToBuild := HelpTopic named: rootToBuildFrom name.
	topicToBuild icon: (self iconNamed: #pageIcon).
	topicToBuild contents: rootToBuildFrom comment.
	addMethods
		ifTrue: [ self buildSubnodesForMethods ].
	addSubclasses
		ifTrue: [ self buildSubnodesForSubclasses ]