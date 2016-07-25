tests
testGoOnMockTutorial
	| displayedText expected |
	ProfStef goOn: MockTutorial.
	
	displayedText := ProfStef default lessonView text.
	expected := MockTutorial new firstLesson lesson.
 	self assert: displayedText equals: expected.