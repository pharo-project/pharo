running
setUp
	| tutorial |
	tutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: (MockTutorial new)