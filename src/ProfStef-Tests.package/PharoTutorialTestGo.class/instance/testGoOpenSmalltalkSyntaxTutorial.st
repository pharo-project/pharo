tests
testGoOpenSmalltalkSyntaxTutorial
	| displayedText expected |
	ProfStef go.
	
	displayedText := ProfStef default lessonView text.
	expected := PharoSyntaxTutorial new welcome lesson.
 	self assert: displayedText equals: expected.