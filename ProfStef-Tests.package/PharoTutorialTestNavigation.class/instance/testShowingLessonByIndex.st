tests
testShowingLessonByIndex
	prof tutorial: MockTutorial lessonAt: 2.
	self assert: mockView title equals: 'second (2/3)'.
	self assert: mockView lesson lesson equals: 'Second lesson'.