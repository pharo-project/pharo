tests
testNextOpenSecondLesson
	prof next.
	self assert: mockView title equals: 'second (2/3)'.
	self assert: mockView lesson lesson equals: 'Second lesson'.