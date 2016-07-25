building
buildTutorialTopicFor: aTutorial
	|tutorialTopic|
	tutorialTopic := HelpTopic named: aTutorial title.	
	
	aTutorial new lessons do:[:aLesson|
		tutorialTopic addSubtopic: (HelpTopic 
									title: aLesson title 
									contents:  aLesson lesson)
	].

	^ tutorialTopic 