navigating
tutorial: aTutorialClass lesson: aSelector
	| tutorial |
	tutorial := aTutorialClass new.
	self player tutorial: tutorial.
	self tutorial: aTutorialClass lessonAt: (tutorial indexOfLesson: aSelector).