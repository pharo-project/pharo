tests
testSequenceNextNextOpenThirdLesson
	prof next; next.
	self assert: mockView title equals: 'third (3/3)'.
	self assert: mockView lesson lesson equals: 'Third lesson'.