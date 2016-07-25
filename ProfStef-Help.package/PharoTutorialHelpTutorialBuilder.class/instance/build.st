building
build
	topicToBuild := HelpTopic named: rootToBuildFrom bookName.
	
	AbstractTutorial tutorials do: [:aTutorial| 
		topicToBuild addSubtopic: (self buildTutorialTopicFor: aTutorial)
	].

	^ topicToBuild.