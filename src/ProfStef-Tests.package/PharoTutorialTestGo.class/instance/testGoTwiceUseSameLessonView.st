tests
testGoTwiceUseSameLessonView
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	ProfStef goOn: PharoSyntaxTutorial.
	self assert: (firstLessonView ==  ProfStef default lessonView).