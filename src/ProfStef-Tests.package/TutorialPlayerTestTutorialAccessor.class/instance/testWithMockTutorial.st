tests
testWithMockTutorial
	| mockTutorial player |
	mockTutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: mockTutorial; yourself.
	self assert: player tutorial equals: mockTutorial.