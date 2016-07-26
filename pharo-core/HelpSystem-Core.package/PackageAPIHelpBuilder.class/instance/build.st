building
build 
	|pTopic|
	topicToBuild := (HelpTopic named: rootToBuildFrom bookName).
	rootToBuildFrom helpPackages do: [:package|
		pTopic := HelpTopic named: package.
		topicToBuild addSubtopic: pTopic.
		self buildPackageTopic: pTopic.
		
	]
