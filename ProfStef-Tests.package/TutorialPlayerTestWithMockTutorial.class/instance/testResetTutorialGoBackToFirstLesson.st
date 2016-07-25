tests
testResetTutorialGoBackToFirstLesson
	player next; next.
	self assert: player currentLesson title equals: 'third'.
	
	player rewind.
	self assert: player currentLesson title equals: 'first'.