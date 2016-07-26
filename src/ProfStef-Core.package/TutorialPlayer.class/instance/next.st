navigating
next
	self tutorialPosition < self size
		ifTrue: [tutorialPosition := tutorialPosition + 1].
	^ self currentLesson