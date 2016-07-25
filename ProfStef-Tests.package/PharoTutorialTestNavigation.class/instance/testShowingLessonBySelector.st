tests
testShowingLessonBySelector
	prof tutorial: MockTutorial lesson: #firstLesson.
	self assert: mockView title equals: 'first (1/3)'.
	self assert: mockView lesson lesson equals: 'First lesson'.