tests
testNewLessonHaveEmptyStringForTitleLesson
	| newLesson  |
	newLesson := Lesson new.
	self assert: newLesson title equals: ''.
	self assert: newLesson lesson equals: ''.