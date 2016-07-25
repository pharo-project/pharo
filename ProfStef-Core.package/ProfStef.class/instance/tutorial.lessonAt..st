navigating
tutorial: aTutorialClass lessonAt: lessonIndex
	self player tutorial: aTutorialClass new.
	self player tutorialPosition: lessonIndex.
	self showCurrentLesson.