tests
testGoTwiceShowLessonViewIfNotVisible
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	firstLessonView close.
	ProfStef goOn: PharoSyntaxTutorial.
	self assert:  (World systemWindows includes:  firstLessonView window).