tests
testLessonsReturnsAllLessonInstances
	|lessons|
	lessons := MockTutorial new lessons.
	self assert:3 equals:lessons size.
	self assert: 'first' equals: lessons first title.
	self assert: 'second' equals: (lessons at:2 ) title.
	self assert: 'third' equals: lessons last title.