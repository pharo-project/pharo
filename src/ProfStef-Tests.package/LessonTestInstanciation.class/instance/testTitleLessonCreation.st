tests
testTitleLessonCreation
	| firstLesson secondLesson |
	firstLesson := Lesson title: 'First lesson' lesson: 'Pharo rules!'.
	secondLesson := Lesson title: 'Second lesson' lesson: 'PharoTutorial is cool'.
	
	self assert: firstLesson title equals: 'First lesson'.
	self assert: firstLesson lesson equals: 'Pharo rules!'.
	
	self assert: secondLesson title equals: 'Second lesson'.
	self assert: secondLesson lesson equals: 'PharoTutorial is cool'.