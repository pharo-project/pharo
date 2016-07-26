running
setUp
	prof := ProfStef new.
	prof player: (
		TutorialPlayer new 
			tutorial: MockTutorial new; 
			yourself).
	mockView := MockLessonView new.
	prof lessonView: mockView.
	prof open.