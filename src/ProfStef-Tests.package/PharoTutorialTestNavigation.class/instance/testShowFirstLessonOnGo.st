tests
testShowFirstLessonOnGo
	| lesson |
	lesson := mockView lesson.
	self assert: lesson title equals: 'first'.
	self assert: lesson lesson equals: 'First lesson'.